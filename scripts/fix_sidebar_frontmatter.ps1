# Script: fix_sidebar_frontmatter.ps1
# Objetivo: pós-processar docs/index.html removendo a <span class="chapter-number">...</span>
# para os arquivos listados como front matter no _quarto.yml (entradas simples antes do primeiro '- part:').
# Uso: execute na raiz do projeto: powershell -ExecutionPolicy Bypass -File .\scripts\fix_sidebar_frontmatter.ps1

# Determinar a raiz do projeto (diretório pai do diretório scripts)
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$projectRoot = Resolve-Path (Join-Path $scriptDir '..')
Set-Location $projectRoot

$yml = Get-Content -Raw -Path "_quarto.yml"
$lines = $yml -split "\r?\n"

# Encontrar bloco 'chapters:' e coletar entradas simples (strings) até encontrar um '- part:'
$inChapters = $false
$frontFiles = @()
foreach ($line in $lines) {
    if (-not $inChapters) {
        if ($line -match '^\s*chapters:\s*$') { $inChapters = $true; continue }
    } else {
        # fim do bloco se encontrar outra chave de topo (ex: 'bibliography:'), ou 'appendices:'
        if ($line -match '^\S') { break }
        # linha com '- part:' indica saída da lista de front matter
        if ($line -match '^\s*-\s*part:\s*') { break }
        # linhas com '- something' são entradas; coletar apenas entradas simples (sem ':')
        if ($line -match '^\s*-\s*(\S.*\S)\s*$') {
            $entry = $Matches[1].Trim()
            # ignorar entradas que são objetos (ex.: part: "...") - aquelas contêm ':' no início ou são seguidas por 'chapters:'
            if ($entry -notmatch ':') {
                $frontFiles += $entry
            }
        }
    }
}

if ($frontFiles.Count -eq 0) {
    Write-Output "Nenhuma entrada de front matter encontrada em _quarto.yml"
    exit 0
}

$indexPath = Join-Path $projectRoot 'docs\index.html'
if (-not (Test-Path $indexPath)) { Write-Error "Arquivo docs/index.html não encontrado. Rode 'quarto render' primeiro."; exit 1 }

$html = Get-Content -Raw -Path $indexPath

# Construir um hashset com os hrefs dos front matter para comparação rápida
$frontHrefs = [System.Collections.Generic.HashSet[string]]::new()
foreach ($f in $frontFiles) {
    $htmlFile = [System.IO.Path]::ChangeExtension($f, '.html')
    $href = "./" + ($htmlFile -replace '\\','/')
    $frontHrefs.Add($href) | Out-Null
}

# Encontrar todas as âncoras do sidebar em ordem e ajustar os spans de numeração
$pattern = '<a[^>]*href="(?<href>\./[^\"]+)"[^>]*class="[^\"]*sidebar-link[^\"]*"[^>]*>(?<inner>.*?)</a>'
$matches = [regex]::Matches($html, $pattern, [System.Text.RegularExpressions.RegexOptions]::Singleline)

# Reconstruir o HTML por partes para evitar problemas com escapes e posições móveis
$result = ''
$lastIndex = 0
$chapterCounter = 1

for ($i = 0; $i -lt $matches.Count; $i++) {
    $m = $matches[$i]
    $start = $m.Index
    $length = $m.Length
    # anexar trecho entre o último match e este
    if ($start -gt $lastIndex) {
        $result += $html.Substring($lastIndex, $start - $lastIndex)
    }

    $href = $m.Groups['href'].Value
    $anchorHtml = $m.Groups['inner'].Value
    $aTag = $m.Value

    if ($frontHrefs.Contains($href)) {
        $newInner = [regex]::Replace($anchorHtml, '<span\s+class="chapter-number">.*?</span>\s*&nbsp;|<span\s+class="chapter-number">.*?</span>', '', [System.Text.RegularExpressions.RegexOptions]::Singleline)
    } else {
        if ($anchorHtml -match '<span\s+class="chapter-number">(.*?)</span>') {
            $numSpan = "<span class=`"chapter-number`">$chapterCounter</span>"
            $newInner = [regex]::Replace($anchorHtml, '<span\s+class="chapter-number">.*?</span>', $numSpan, [System.Text.RegularExpressions.RegexOptions]::Singleline)
        } else {
            $newInner = "<span class=`"chapter-number`">$chapterCounter</span>&nbsp; $anchorHtml"
        }
        $chapterCounter++
    }

    if ($newInner -ne $anchorHtml) {
        # substituir somente a parte inner dentro da tag <a>
        $innerIndex = $aTag.IndexOf($anchorHtml)
        if ($innerIndex -ge 0) {
            $aTagNew = $aTag.Substring(0, $innerIndex) + $newInner + $aTag.Substring($innerIndex + $anchorHtml.Length)
        } else {
            # fallback: usar a tag original
            $aTagNew = $aTag
        }
        $result += $aTagNew
        Write-Output "Atualizado sidebar para: $href"
    } else {
        $result += $aTag
    }

    $lastIndex = $start + $length
}

# anexar resto do documento
if ($lastIndex -lt $html.Length) { $result += $html.Substring($lastIndex) }

# substituir o html pelo resultado processado
$html = $result

# salvar o html modificado (sobrepondo docs/index.html)
Set-Content -Path $indexPath -Value $html -Encoding UTF8
Write-Output "docs/index.html atualizado com sucesso."
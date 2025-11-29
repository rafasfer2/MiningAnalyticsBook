# Script: fix_sidebar_frontmatter.ps1
# Objetivo: pós-processar docs/index.html removendo a <span class="chapter-number">...</span>
# para os arquivos listados como front matter no _quarto.yml (entradas simples antes do primeiro '- part:').
# Uso: execute na raiz do projeto: powershell -ExecutionPolicy Bypass -File .\scripts\fix_sidebar_frontmatter.ps1

$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition
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

foreach ($f in $frontFiles) {
    # normalizar nome para .html (mantém subpastas se presentes)
    $htmlFile = [System.IO.Path]::ChangeExtension($f, '.html')
    # garantir barra './' prefixo na comparação (em index.html os hrefs usam './')
    $href = "./" + $htmlFile -replace '\\', '/'

    # localizar a posição do href no HTML
    $pos = $html.IndexOf($href, [System.StringComparison]::OrdinalIgnoreCase)
    if ($pos -lt 0) { continue }

    # encontrar o início da tag <a mais próxima antes do href
    $aStart = $html.LastIndexOf('<a', $pos)
    if ($aStart -lt 0) { continue }
    $aEnd = $html.IndexOf('</a>', $pos)
    if ($aEnd -lt 0) { continue }
    $aEnd += 4 # incluir </a>

    $anchor = $html.Substring($aStart, $aEnd - $aStart)

    # remover o span chapter-number dentro da âncora
    $newAnchor = $anchor -replace '<span\s+class="chapter-number">.*?</span>\s*&nbsp;','' -replace '<span\s+class="chapter-number">.*?</span>',''

    if ($newAnchor -ne $anchor) {
        $html = $html.Substring(0, $aStart) + $newAnchor + $html.Substring($aEnd)
        Write-Output "Atualizado sidebar para: $href"
    }
}

# salvar o html modificado (sobrepondo docs/index.html)
Set-Content -Path $indexPath -Value $html -Encoding UTF8
Write-Output "docs/index.html atualizado com sucesso."
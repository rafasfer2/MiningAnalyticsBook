# Substitui nos HTML gerados em docs/:
# 1) "Figura X.Y: ..." -> "Figura X.Y - ..."
# 2) "Exemplo X.Y (Título)" -> "Exemplo X.Y - Título"

$root = Join-Path $PSScriptRoot '..\docs'
if (-not (Test-Path $root)) {
    Write-Error "Pasta docs/ não encontrada: $root"
    exit 1
}

Get-ChildItem -Path $root -Recurse -Filter *.html | ForEach-Object {
    $path = $_.FullName
    $orig = Get-Content -Raw -Encoding UTF8 $path
    $content = $orig

    # Substitui prefixos de figuras/tabelas (considera &nbsp; e espaços)
    $content = $content -replace '((Figura|Tabela)\s*(?:&nbsp;|\s*)\d+(?:\.\d+)*)\s*[:：]\s*', '$1 - '

    # Substitui títulos de exemplo com parênteses: Exemplo 1.1 (Título) -> Exemplo 1.1 - Título
    $content = $content -replace 'Exemplo\s*(\d+(?:\.\d+)*)\s*\(', 'Exemplo $1 - '
    # Remove parênteses de fechamento quando estiverem imediatamente antes de tags de fechamento fortes
    $content = $content -replace '\)\s*(</strong>)', '$1'

    if ($content -ne $orig) {
        Set-Content -Path $path -Value $content -Encoding UTF8
        Write-Output "Atualizado: $path"
    }
    else {
        Write-Output "Sem alterações: $path"
    }
}
# Substitui nos HTML gerados em docs/:
# 1) "Figura X.Y: ..." -> "Figura X.Y - ..."
# 2) "Exemplo X.Y (Título)" -> "Exemplo X.Y - Título"

$root = Join-Path $PSScriptRoot '..\\docs'
if (-not (Test-Path $root)) {
    Write-Error "Pasta docs/ não encontrada: $root"
    exit 1
}

Get-ChildItem -Path $root -Recurse -Filter *.html | ForEach-Object {
    $path = $_.FullName
    $orig = Get-Content -Raw -Encoding UTF8 $path
    $content = $orig

    # Substitui prefixos de figuras/tabelas (considera &nbsp; e espaços)
    $content = $content -replace '((Figura|Tabela)\s*(?:&nbsp;|\s*)\d+(?:\.\d+)*)\s*[:：]\s*', '$1 - '

    # Substitui títulos de exemplo com parênteses: Exemplo 1.1 (Título) -> Exemplo 1.1 - Título
    $content = $content -replace 'Exemplo\s*(\d+(?:\.\d+)*)\s*\(', 'Exemplo $1 - '
    # Remove parênteses de fechamento quando estiverem imediatamente antes de tags de fechamento fortes
    $content = $content -replace '\)\s*(</strong>)', '$1'

    if ($content -ne $orig) {
        Set-Content -Path $path -Value $content -Encoding UTF8
        Write-Output "Atualizado: $path"
    }
    else {
        Write-Output "Sem alterações: $path"
    }
}

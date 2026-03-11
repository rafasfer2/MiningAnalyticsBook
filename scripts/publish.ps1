[CmdletBinding()]
param(
    [Parameter(Mandatory = $false)]
    [string]$Message,

    [Parameter(Mandatory = $false)]
    [switch]$SkipRender,

    [Parameter(Mandatory = $false)]
    [switch]$AllowEmpty,

    [Parameter(Mandatory = $false)]
    [switch]$DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Invoke-Step {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Title,

        [Parameter(Mandatory = $true)]
        [scriptblock]$Action
    )

    Write-Host "`n==> $Title" -ForegroundColor Cyan
    & $Action
}

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    throw "Git nao encontrado no PATH."
}

if (-not (Get-Command quarto -ErrorAction SilentlyContinue) -and -not $SkipRender) {
    throw "Quarto nao encontrado no PATH. Use -SkipRender para pular render."
}

$repoRoot = git rev-parse --show-toplevel 2>$null
if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($repoRoot)) {
    throw "Este comando precisa ser executado dentro de um repositorio Git."
}

Set-Location $repoRoot

$branch = (git rev-parse --abbrev-ref HEAD).Trim()
if ([string]::IsNullOrWhiteSpace($branch)) {
    throw "Nao foi possivel identificar a branch atual."
}

if (-not $SkipRender) {
    Invoke-Step -Title "Renderizando projeto Quarto" -Action {
        if ($DryRun) {
            Write-Host "[dry-run] quarto render"
        }
        else {
            quarto render
        }
    }
}

Invoke-Step -Title "Adicionando alteracoes" -Action {
    if ($DryRun) {
        Write-Host "[dry-run] git add -A"
    }
    else {
        git add -A
    }
}

$status = git status --short
$hasChanges = -not [string]::IsNullOrWhiteSpace(($status | Out-String).Trim())

if (-not $hasChanges -and -not $AllowEmpty) {
    Write-Host "Nada para commitar. Repositorio ja esta limpo." -ForegroundColor Yellow
    exit 0
}

if ([string]::IsNullOrWhiteSpace($Message)) {
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $Message = "Atualizacao automatica: render e publicacao ($timestamp)"
}

Invoke-Step -Title "Criando commit" -Action {
    if ($DryRun) {
        Write-Host "[dry-run] git commit -m \"$Message\""
    }
    else {
        if ($AllowEmpty) {
            git commit --allow-empty -m "$Message"
        }
        else {
            git commit -m "$Message"
        }
    }
}

Invoke-Step -Title "Enviando para origin/$branch" -Action {
    if ($DryRun) {
        Write-Host "[dry-run] git push origin $branch"
    }
    else {
        git push origin $branch
    }
}

Write-Host "`nPublicacao finalizada com sucesso." -ForegroundColor Green

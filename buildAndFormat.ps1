$main = '.\js\main.js'
$minified = '.\js\main.min.js'
elm-format .\src --yes
elm make .\src\Main.elm --optimize --output $main
uglifyjs $main --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $minified
$item = Get-Item $main
$itemSize = $item.Length/1KB
$minifiedItem = Get-Item $minified
$minifiedItemSize = $minifiedItem.Length/1KB
Write-Host("$main -> $itemSize KB")
Write-Host("$minified -> $minifiedItemSize KB")
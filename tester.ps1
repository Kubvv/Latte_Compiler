if (Test-Path -Path .\tests_raport.txt -PathType Leaf) {
  Remove-Item -Path .\tests_raport.txt
}
Get-ChildItem -Path .\ -Filter *.lat -Recurse -File -Name | ForEach-Object {
  Add-Content .\tests_raport.txt "<=======================>"
  Add-Content .\tests_raport.txt ($_)
  $allOutput = & .\latc.exe ($_) 2>&1
  $stderr = $allOutput | ?{ $_ -is [System.Management.Automation.ErrorRecord] }
  $stdout = $allOutput | ?{ $_ -isnot [System.Management.Automation.ErrorRecord] }
  Add-Content .\tests_raport.txt "<== Stderr ==>"
  Add-Content .\tests_raport.txt $stderr
  Add-Content .\tests_raport.txt "<== Stdout ==>"
  Add-Content .\tests_raport.txt $stdout
  Add-Content .\tests_raport.txt "`n"
}
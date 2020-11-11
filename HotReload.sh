PKG=dbstorage-polysemy

sigint_handler()
{
  kill $(jobs -p)
  exit
}

trap sigint_handler SIGINT

while true; do
  # $@ &
  ghcid -l -c "cabal new-repl $PKG --builddir dist-$PKG" & 
 
  # run_server &
  PID1=$!

  inotifywait \
      -e modify \
      -e move \
      -e create \
      -e delete \
      -e attrib \
      $PKG.cabal \
      --exclude ".*flycheck.*|.*\#.*"
  kill $(jobs -p)
  sleep 3
done


if ($line =~ m{
               ^                       # Anchor at beginning
               zapp
               $                       # Anchor at end
              }xo) {
    
    zapp
}
else {
    die "Expected zapp, got $line";
}

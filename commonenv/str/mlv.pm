elsif ($line =~ m{
                  ^                       # Anchor at beginning
                  zapp
                  $                       # Anchor at end
                 }xo) {
    zapp
}

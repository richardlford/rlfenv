while ($line =~ m{
                  ^                       # Anchor at beginning
                  zapp
                  $                       # Anchor at end
                 }xo) {
    
    zapp

    $line = &$get_line();
    die "Premature end to CVS status info" if !$line;
    chomp($line);
}

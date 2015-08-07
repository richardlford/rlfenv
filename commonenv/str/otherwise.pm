try{
    zapp
}
otherwise{
    my $E = shift;
    confess $E->{-text};
};

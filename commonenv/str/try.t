try{
    ok(1, 'In try ?');
    zapp
    ok(0, 'Got past error statement ?');
}
otherwise{
    my $E = shift;
    #print Dumper($E);
    isa_ok($E, 'Error::Simple', 'E ?');
    ok($E->{-text} =~ /zapp/, 'Expected error matched ?');
};

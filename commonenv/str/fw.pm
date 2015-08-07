sub [[sub-name]] {
    shift; # Remove 'Fwd'
    return Fwd->to(File::Spec->[[sub-name]](@_));
}

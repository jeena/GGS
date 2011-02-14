def greatest_sequence(match, pattern):
    m = match
    p = pattern
    size = 0
    max_size = 0

    for p in pattern:
        if m == p:
            size += 1
        else:
            if size > max_size:
                max_size = size
                size = 0

    return max_size

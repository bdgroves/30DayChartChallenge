"""Spread label positions apart along one axis (for slope-chart ends and the like)."""


def spread(values, gap):
    """Return positions as close to `values` as possible, at least `gap` apart, same order."""
    order = sorted(range(len(values)), key=lambda i: values[i])
    pos = [values[i] for i in order]
    for _ in range(200):
        moved = False
        for k in range(1, len(pos)):
            d = pos[k] - pos[k - 1]
            if d < gap:
                push = (gap - d) / 2
                pos[k - 1] -= push; pos[k] += push; moved = True
        if not moved:
            break
    out = [0.0] * len(values)
    for k, i in enumerate(order):
        out[i] = pos[k]
    return out

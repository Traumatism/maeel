_Definitions:_

- $swap:(\dots, a, b) \mapsto (\dots, b, a)$
- $dup:(\dots, a) \mapsto (\dots, a, a)$
- $rot:(\dots, a, b) \mapsto (\dots, b, a)$
- $drop:(\dots, a) \mapsto (\dots)$
- $I:S \mapsto S$

_Properties:_

- $swap \circ swap \equiv I$
- $rot \circ rot \circ rot \equiv I$
- $rot \circ swap \circ rot \equiv swap$
- $swap \circ rot \circ swap \equiv rot \circ rot$
# An area-bounce exchanging bijection on a large subset of Dyck paths

This repository implements the bijection presented [here](https://arxiv.org/abs/2401.14668).

## Examples
The [examples folder](https://github.com/nanonaren/qtcatalan-bijection/tree/main/examples) contains the bijection enumerated for various n. These are contained in the files named `nk.txt`. Each row in this file looks like

```
True 9 2 [0,1,2,3,1,2] [0,0,1,0,1,0]
```
The columns are as follows
* The 4th column is a dyck path d having area >= bounce.
* The 2nd column is the area of d.
* The 3rd column is the bounce of d.
* The 5th column is Phi(d).
* The first column is `True` if the bounce value is minimal given the area, i.e., if d in B(n).

The [minimal_counts.txt](https://github.com/nanonaren/qtcatalan-bijection/blob/main/examples/minimal_counts.txt) contains the number of paths in B(n). For example, we can verify that every path in B(10) has been mapped by Phi as follows

```bash
> cat examples/minimal_counts.txt | grep "^10"
10 69

> cat examples/n10.txt | grep True | wc -l
69
```

## Map any path in B(n)

Download the executable `phi-linux` from [here](https://github.com/nanonaren/qtcatalan-bijection/releases/tag/v0.1.0). For example, to compute Phi of the path `[0,1,2,3,4,5,6,2,2,2,0]` in n=11

```bash
> echo "[0,1,2,3,4,5,6,2,2,2,0]" | ./phi-linux
Just [0,1,2,0,0,1,0,0,1,0,0]
```

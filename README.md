# An area-bounce exchanging bijection on a large subset of Dyck paths

This repository implements the bijection presented [here](https://arxiv.org/abs/2401.14668), Section 3. It also verifies that bounce-minimal paths are mapped to area-minimal paths under this bijection.

Dyck paths are represented as lists of 0s and 1s, where 0 represents an up step and 1 represents a right step.

## Examples
The [examples folder](https://github.com/nanonaren/qtcatalan-bijection/tree/main/examples) contains the bijection enumerated for various values of $n$. These are contained in the files named `nk.txt`, where k is the size. Each row in this file looks like

```
[0,1,2,3,1,2] [0,0,1,0,1,0] 9 2 True
```
The columns are as follows
* The 1st column is a Dyck path ![equation](https://latex.codecogs.com/svg.image?d\in\mathcal{AF}_n).
* The 2nd column is $\Phi(d)$.
* The 3rd column is the area of $d$.
* The 4th column is the bounce of $d$.
* The 5th column is `True` if $d$ is bounce-minimal, i.e., if ![equation](https://latex.codecogs.com/svg.image?d\in\mathcal{B}(10)) and equivalently ![equation](https://latex.codecogs.com/svg.image?\Phi(d)\in\mathcal{A}_n).

The [minimal_counts.txt](https://github.com/nanonaren/qtcatalan-bijection/blob/main/examples/minimal_counts.txt) contains the number of paths in $\mathcal{B}(n)$. For example, we can verify that every path in ![equation](https://latex.codecogs.com/svg.image?\mathcal{B}(10)) has been mapped by $\Phi$ as follows

```bash
> cat examples/minimal_counts.txt | grep "^10"
10 69

> cat examples/n10.txt | grep True | wc -l
69
```

## Map any path in ![equation](https://latex.codecogs.com/svg.image?\mathcal{AF}_n)

Download the executable `phi-linux` from [here](https://github.com/nanonaren/qtcatalan-bijection/releases/tag/v0.1.0). For example, to compute $\Phi$ of some paths

```bash
> echo -e "[0,1,2,3,4,5]\n[0,1,2,3,4,5,6,2,2,2,0]\n[0,1,2,2,2,2]\n[0,1,2,3,1,1]" | cabal exec phi-code
Just [0,0,0,0,0,0]
Just [0,1,2,0,0,1,0,0,1,0,0]
Nothing
Just [0,1,0,0,1,0]
```

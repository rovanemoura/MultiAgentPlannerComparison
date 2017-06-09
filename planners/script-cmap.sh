cd cmap
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r1/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r2/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r3/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r4/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r5/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r6/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r7/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r8/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r9/cmap/p10.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p01/domain.pddl ../benchmarks/unfactored/floods/p01/problem.pddl output.out ; } 2>&1 | cat > p01temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p01.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p02/domain.pddl ../benchmarks/unfactored/floods/p02/problem.pddl output.out ; } 2>&1 | cat > p02temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p02.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p03/domain.pddl ../benchmarks/unfactored/floods/p03/problem.pddl output.out ; } 2>&1 | cat > p03temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p03.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p04/domain.pddl ../benchmarks/unfactored/floods/p04/problem.pddl output.out ; } 2>&1 | cat > p04temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p04.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p05/domain.pddl ../benchmarks/unfactored/floods/p05/problem.pddl output.out ; } 2>&1 | cat > p05temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p05.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p06/domain.pddl ../benchmarks/unfactored/floods/p06/problem.pddl output.out ; } 2>&1 | cat > p06temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p06.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p07/domain.pddl ../benchmarks/unfactored/floods/p07/problem.pddl output.out ; } 2>&1 | cat > p07temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p07.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p08/domain.pddl ../benchmarks/unfactored/floods/p08/problem.pddl output.out ; } 2>&1 | cat > p08temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p08.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p09/domain.pddl ../benchmarks/unfactored/floods/p09/problem.pddl output.out ; } 2>&1 | cat > p09temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p09.out; done
rm *.out
{ time timeout 60m ./plan ../benchmarks/unfactored/floods/p10/domain.pddl ../benchmarks/unfactored/floods/p10/problem.pddl output.out ; } 2>&1 | cat > p10temp.out
for f in *.out; do ( cat "${f}"; echo) >> ../results/r10/cmap/p10.out; done
rm *.out

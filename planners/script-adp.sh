cd adp
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r2/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r3/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r4/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r5/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r6/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r7/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r8/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r9/adp/p10.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p01.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p02.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p03.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p04.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p05.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p06.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p07.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p08.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p09.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r10/adp/p10.out; done
rm ../*.out

cd marc
{ time timeout 60m ./plan.sh floods p01 ; } 2>&1 | cat > ../p01temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p01-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p02 ; } 2>&1 | cat > ../p02temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p02-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p03 ; } 2>&1 | cat > ../p03temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p03-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p04 ; } 2>&1 | cat > ../p04temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p04-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p05 ; } 2>&1 | cat > ../p05temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p05-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p06 ; } 2>&1 | cat > ../p06temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p06-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p07 ; } 2>&1 | cat > ../p07temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p07-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p08 ; } 2>&1 | cat > ../p08temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p08-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p09 ; } 2>&1 | cat > ../p09temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p09-gppp1.out; done
rm ../*.out
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/marc/p10-gppp1.out; done
rm ../*.out

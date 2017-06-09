cd planner1
{ time timeout 60m ./plan.sh floods p10 ; } 2>&1 | cat > ../p10temp.out
for f in ../*.out; do ( cat "${f}"; echo) >> ../results/r1/p10-gppp1.out; done
rm ../*.out

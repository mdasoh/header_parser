yacc -d 0008.y
flex 0007.l
gcc -o parser y.tab.c lex.yy.c -lfl

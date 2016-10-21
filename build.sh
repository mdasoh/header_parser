yacc -d 0006.y
flex 0006.l
gcc -o parser y.tab.c lex.yy.c -lfl

#if, else, else if
x= 5

if( x> 20){
  r= "x é maior que 20"
  print(r)
} else if (x==20){
  r= "x é igual a 20"
  print(r)
}else{
  print(x)
  "é menor que 20"
}

#decomposição
y= 23
frase= if(y<18) "Pode ir embora" else "Pode entrar"
print(frase)


#while
x= 20
while(x<30){
  x=x+2
  print(x)
}

#for
for(i in 1:10){
  print(i)
}

#Substituindo valores de uma matriz usando for
a= matrix(0,10,1)
for (i in 1:10) {
  a[i]=i+2
  print(a)
}

#exercícios
#crie uma matriz A que possua dimensões 4x4,
#mostre somente os elementos de posições onde i e j 
#sejam valores pares

a= matrix(1:16, 4,4)
for(i in 1:4){
 for(j in 1:4){
   if(i%%2==0 && j%%2==0){
     print(a[i,j])
   }
 }
}

#escreva 'tem' se houver um número par na matriz
h = 1:10
#h%%2==0
for (i in 1:10) {
  if(h[i]%%2==0){
    print("tem")
  }
}
#print o valor de f em que o valor de f*2 é menor que 8

f= 4

if(f*2<8){
  print("f*2 é menor que 8")
} else if(f*2 == 8){
  print("f*2 é igual a 8")
} else{
  print("f*2 é maior que 8")
}

for (f in 1:20) {
  if(f*2<15){
    print(f)
  }
}

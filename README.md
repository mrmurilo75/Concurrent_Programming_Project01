# Trabalho 1

O trabalho consiste em: 
 - programa em Erlang
 - pequeno relatório de **duas páginas** *no máximo*
   - Introducão
   - Explicação de funcionamento
   - Ilustração de funcionamento a casos de teste (escolhido pelo aluno)

O trabalho é realizado em grupos de dois alunos.

## Envio
 - Enviado por **email para _amflorid (at) fc.up.pt_**
 - Com o **Subject: Programação Concorrente - Trabalho Prático**
 - Até ao final do dia **3 de Abril**
 - **Apresentado na aula prática** da semana **seguinte** (4 a 8 de Abril)

## Enunciado
 1. Implemente em Erlang um programa
    - modelo cliente-servidor
    1. O server deverá:
       - calcular operações entre polinómios
         * somas
         * subtrações
         * multiplicações
       - Represente polinómios como listas de tuplos
         * {Variável, Coeficiente, Expoente}
    2. Os vários clients podem:
       - enviar os pedidos correspondentes ao server
       - receber a resposta
       - apresentar os resultados.
    - > Valorização: 3 valores.
 2. Adapte o programa para correr em nós diferentes (Server e Clients) num ambiente distribuído de programação
    - Consulte o capítulo 14 do livro Programming Erlang, Software for a Concurrent World
    - > Valorização: 1 valor.


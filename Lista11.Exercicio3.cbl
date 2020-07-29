      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "Lista11.Exercicio3".
       author. "Jade Rogelin ".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.

      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

                  select arqCadAluno assign to "arqCadAluno.txt" *> adiciona nome arquivo    *> assing vou estar assossiando o arquivo fisico
                  organization is indexed                         *> forma de como sao organizados os dados/aruivo
                  access mode is dynamic                          *> assesaando o aquivo/dados
                  lock mode is automatic                          *> serve para travar o arquivo
                  record key is fd-cod                            *> chave relativa para acesso randomico (acesso direto)
                  file status is ws-fs-arqCadAluno.               *> é utilizado uma variavel da wokking-storage para retorno correto do aqruivo

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqCadAluno.
       01 fd-alunos.
           05  fd-cod                              pic X(03).
           05  fd-aluno                            pic X(25).
           05  fd-endereco                         pic X(35).
           05  fd-mae                              pic X(25).
           05  fd-pai                              pic X(25).
           05  fd-telefone                         pic X(15).
           05  fd-notas.
               10  fd-nota1                        pic 9(02)v99
                                                   value 11.
               10  fd-nota2                        pic 9(02)v99
                                                   value 11.
               10  fd-nota3                        pic 9(02)v99
                                                   value 11.
               10  fd-nota4                        pic 9(02)v99
                                                   value 11.


      *>----Variaveis de trabalho
       working-storage section.

       77 ws-fs-arqCadAluno                        pic 9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 ws-msn-erro-cod                       pic 9(02).
          05 ws-msn-erro-text                      pic X(42).

       01  ws-alunos.
           05  ws-aluno                            pic X(25).
           05  ws-cod                              pic X(03).
           05  ws-endereco                         pic X(35).
           05  ws-mae                              pic X(25).
           05  ws-pai                              pic X(25).
           05  ws-telefone                         pic X(15).
           05  ws-notas.
               10  ws-nota1                        pic 9(02)v99
                                                   value 11.
               10  ws-nota2                        pic 9(02)v99
                                                   value 11.
               10  ws-nota3                        pic 9(02)v99
                                                   value 11.
               10  ws-nota4                        pic 9(02)v99
                                                   value 11.

       77 ws-sair                                  pic X(01).
       77 ws-menu                                  pic X(02).

      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

           open i-o arqCadAluno   *> open i-o abre o arquivo para leitura e escrita
           if ws-fs-arqCadAluno  <> 0
           and ws-fs-arqCadAluno <> 05 then
               move 1                                 to ws-msn-erro-ofsset
               move ws-fs-arqCadAluno                 to ws-msn-erro-cod
               move "Erro ao abrir arq. arqCadAluno " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

      *>    inicializa menu
           move  spaces      to     ws-menu
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until ws-sair = "X"
                      or ws-sair = "x"
               *> menu
               display erase
               display "'CA'dastrar Aluno"
               display "'NO'tas"
               display "'CO'nsulta indexada"
               display "'CS'nsulta sequencial"
               display "'DE'letar"
               display "'AL'terar"
               display "'S'air"
               accept ws-menu

               evaluate  ws-menu
                  when = 'CA'
                  when = 'ca'
                       perform cadastrar-aluno

                   when = 'NO'
                   when = 'no'

                       perform cadastrar-notas

                   when = 'CO'
                   when = 'co'

                       perform consulta-indexada

                   when = 'CS'
                   when = 'cs'

                       perform consulta-sequencial

                   when = 'DE'
                   when = 'de'

                       perform deletar-cadastro

                   when = 'AL'
                   when = 'al'

                       perform alterar-cadastro

                   when = 'S'
                   when = 's'
                       display "Finalizando programa de cadastros"

                   when other
                       display "Opcao invalida!!!"
               end-evaluate

           end-perform


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de aluno
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

            perform until ws-sair = "V"
                       or ws-sair = "v"

           display erase
           display "-------  cadastro de alunos -------"
           display "Codigo do Aluno: "
           accept ws-cod
           display "Nome Aluno(a)  : "
           accept ws-aluno
           display "Endereco       : "
           accept ws-endereco
           display "Nome da mae    : "
           accept ws-mae
           display "Nome do pai    : "
           accept ws-pai
           display "Telefone       : "
           accept ws-telefone

      *> -------------  Salvar dados no arquivo
               write fd-alunos       from ws-alunos
               if ws-fs-arqCadAluno <> 0 then
                   move 2                                    to ws-msn-erro-ofsset
                   move ws-fs-arqCadAluno                    to ws-msn-erro-cod
                   move "Erro ao escrever arq. arqCadAluno " to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
      *> -------------

           display "  "
           display "Deseja cadastrar mais um Aluno? 'S'im ou 'V'oltar"
           accept ws-sair


           .
       cadastrar-aluno-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           perform until ws-sair = "V"
                      or ws-sair = "v"
           *> menu para o usuario cadastrar notas
           display erase
           display "------ Cadastro de notas ------"
           display "Informe o cod. do aluno : "
           accept fd-cod

           display "Informe a primeira nota : "
           accept ws-nota1

           display "Informe a segunda nota  : "
           accept ws-nota2

           display "Informe a terceira nota : "
           accept ws-nota3

           display "Informe a quarta nota   : "
           accept ws-nota4

           display "  "
           display "Deseja cadastrar notas? 'S'im ou 'V'oltar"
           accept ws-sair


           move ws-cod       to fd-cod
           *> para ler as variavies de arquivo
           read arqCadAluno
           if  ws-fs-arqCadAluno <> 0
           and ws-fs-arqCadAluno <> 23 then

           move ws-fs-arqCadAluno to ws-menu

           move ws-notas to fd-notas
           *> para sobreescrever um registro
           rewrite fd-alunos
           if ws-fs-arqCadAluno <> 0

           .
       cadastrar-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consultar cadastro indexado
      *>------------------------------------------------------------------------
       consulta-indexada section.


      *> -------------  Ler dados do arquivo
               display "informe o codigo do aluno: "
               accept fd-cod

               move ws-alunos to fd-alunos

               read arqCadAluno
               if  ws-fs-arqCadAluno <> 0
               and ws-fs-arqCadAluno <> 10 then
                   if ws-fs-arqCadAluno = 23 then
                       display "Codigo informado invalido!"
                   else
                       move 3                                       to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                       to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAluno "         to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

               move  fd-alunos       to  ws-alunos

               display "Codigo     : " ws-cod
               display "Aluno      : " ws-aluno
               display "Endereco   : " ws-endereco
               display "Nome da mae: " ws-mae
               display "Nome do pai: " ws-pai
               display "Telefone   : " ws-telefone

           .
       consulta-indexada-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  consultar cadastro sequencial
      *>------------------------------------------------------------------------
       consulta-sequencial section.

           *>perform consulta-indexada

           perform until ws-sair = "V"
                      or ws-sair = "v"

      *> -------------  Ler dados do arquivo
               read arqCadAluno next
               if  ws-fs-arqCadAluno <> 0
               and ws-fs-arqCadAluno <> 10 then
                   if ws-fs-arqCadAluno = 23 then
                       display "Data informada invalida!"
                   else
                       move 4                                       to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                       to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAluno "         to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

               move  fd-alunos       to  ws-alunos

               display "Codigo     : " ws-cod
               display "Aluno      : " ws-aluno
               display "Endereco   : " ws-endereco
               display "Nome da mae: " ws-mae
               display "Nome do pai: " ws-pai
               display "Telefone   : " ws-telefone

               display "Deseja realizar mas uma consulta sequencial? 'S'im ou 'V'oltar"
               accept ws-sair
           end-perform

           .
       consulta-sequencial-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Deletar Cadastro
      *>------------------------------------------------------------------------
       deletar-cadastro section.

      *> -------------  Apagar dados do registro do arquivo
               display "informe o cod a ser excluido:"
               accept ws-alunos

               move ws-aluno to fd-aluno
               delete arqCadAluno
               if  ws-fs-arqCadAluno <> 0 then
                   if ws-fs-arqCadAluno = 23 then
                       display "Cod informado invalido!"
                   else
                       move 5                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                       move "Erro ao deletar arq. arqCadAluno " to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

           display "Deseja deletar mais um cadastro? 'S'im ou 'V'oltar"
           accept ws-sair


           .
       deletar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Alterar Cadastro
      *>------------------------------------------------------------------------
       alterar-cadastro section.

               *> ponteiro
               perform consulta-indexada

      *> -------------  Alterar dados do registro do arquivo
               display "Informe novo aluno a ser cadastrado: "
               accept ws-aluno

               move ws-alunos to fd-alunos
               *> sobreescreve o arquivo
               rewrite fd-alunos
               if  ws-fs-arqCadAluno = 0 then
                   display "Novo aluno  " ws-aluno " Cadastrado com sucesso!"
               else
                   move 6                                    to ws-msn-erro-ofsset
                   move ws-fs-arqCadAluno                    to ws-msn-erro-cod
                   move "Erro ao alterar arq. arqCadAluno "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           display "Deseja alterar mais um Aluno? 'S'im ou 'V'oltar"
           accept ws-sair


           .
       alterar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.

           display ws-msn-erro.
           accept ws-msn-erro.

           Stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqCadAluno
           if ws-fs-arqCadAluno <> 0 then
               move 7                                  to ws-msn-erro-ofsset
               move ws-fs-arqCadAluno                  to ws-msn-erro-cod
               move "Erro ao fechar arq. arqCadAluno " to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           Stop run
           .
       finaliza-exit.
           exit.


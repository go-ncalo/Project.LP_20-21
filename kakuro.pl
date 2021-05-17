% Goncalo Mateus, 99225

:- [codigo_comum].

%                           KAKURO SOLVER
%          ================================================
%          =                Inicializacao                 =
%          ================================================           
% -------------------------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs)
% N e um inteiro, Els e uma lista de inteiros e Soma e um inteiro.
% Combs e a lista ordenada cujos elementos sao as combinacoes
% N a N, dos elementos de Els cuja soma e Soma.
% -------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, combinacao(N, Els, Comb), L_Comb),
    % encontra todas as combinacoes que sao membros da lista de
    % combinacoes cuja soma dos elementos seja igual a Soma.
    findall(Comb, 
        (member(Comb, L_Comb), sum_list(Comb, Sum), Sum == Soma),
        Combs).

% -------------------------------------------------------------------
% permutacoes_soma(N, Els, Soma, Perms)
% N e um inteiro, Els e uma lista de inteiros, e Soma e um inteiro.
% Perms e a lista ordenada cujos elementos sao as permutacoes das
% combinacoes N a N, dos elementos de Els cuja soma e Soma.
% -------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm,
        (member(Comb, Combs), permutation(Comb, Perm)),
        Perms_Desordenados),
    sort(Perms_Desordenados, Perms).

% -------------------------------------------------------------------
%                           ESTRUTURA ESPACO                         
% -------------------------------------------------------------------
% cria_espaco(S, Lst, espaco(S, Lst)) - cria um novo espaco onde S e
% a soma do espaco e Lst a sua lista de variaveis.
%
% get_Lst(espaco(_, Lst), Lst) - retorna Lst, a lista de variaveis do
% espaco.
%
% get_S(espaco(S, _), S) - retorna S, a soma do espaco.
%
% change_lst(Lst, espaco(S, _), espaco(S, Lst)) - altera Lst, a lista
% de variaveis de um espaco.
% -------------------------------------------------------------------
cria_espaco(S, Lst, espaco(S, Lst)).
get_Lst(espaco(_, Lst), Lst).
get_S(espaco(S, _), S).
change_lst(Lst, espaco(S, _), espaco(S, Lst)).

% -------------------------------------------------------------------
% espaco_fila(Fila, Esp, H_V)
% Fila e uma fila (linha ou coluna) de um puzzle e H_V e um dos 
% atomos h ou v, conforme se trate de uma fila horizontal ou vertical
% respetivamente. Esp e um espaco de Fila.
%
% espaco_fila_aux(Fila, Esp, Lst, Lst_aux, H_V)
% Predicado auxiliar para a espaco_fila que obtem, recursivamente, o
% Esp de uma Fila.
%
% getS(H_V, P, S)
% Retorna a soma correta de acordo com o atomo H_V.
% -------------------------------------------------------------------

espaco_fila(Fila, Esp, H_V) :-
    espaco_fila_aux(Fila, Esp_aux, [], [], H_V),
    % os espacos vem em ordem contraria, dai ser necessario inverter
    % a sua lista
    reverse(Esp_aux, Esp_list),
    % retira da lista de espacos, todos os espacos cuja lista de
    % variaveis seja vazia
    bagof(Esp, 
    (member(Esp, Esp_list), get_Lst(Esp, Lst), Lst \== []),
    Esp_lst),
    member(Esp, Esp_lst).

% caso base
espaco_fila_aux([], Esp, Esp, _, _).

espaco_fila_aux([P | Q], Esp, Lst_Esp, _, H_V) :-
    % se o primeiro elemento da Fila for uma lista, cria-se um novo
    % espaco
    is_list(P),
    !,
    getS(H_V, P, S),
    cria_espaco(S, [], Esp_aux),
    espaco_fila_aux(Q, Esp, [Esp_aux | Lst_Esp], [], H_V).

espaco_fila_aux([P | Q], Esp, [Esp_at | Resto], Lst, H_V) :-
    % se o primeiro elemento da Fila nao for uma lista, altera-se a
    % a lista de variaveis do espaco, adicionando mais uma variavel
    append(Lst, [P], Lst_aux),
    change_lst(Lst_aux, Esp_at, Esp_no),
    espaco_fila_aux(Q, Esp, [Esp_no | Resto], Lst_aux, H_V).

getS(H_V, P, S) :- H_V \== h -> nth0(0, P, S); nth0(1, P, S).

% -------------------------------------------------------------------
% espacos_fila(H_V, Fila, Espacos)
% Fila e uma fila (linha ou coluna) de uma grelha e H_V e um dos
% atomos h ou v. Espacos e a lista de todos os espacos de Fila,
% da esquerda para a direita.
% -------------------------------------------------------------------
espacos_fila(_, Fila, Espacos) :-
    % se a Fila nao tiver variaveis, Espacos e uma lista vazia
    exclude(is_list, Fila, Lst),
    Lst = [],
    !,
    Espacos = Lst.

espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

% -------------------------------------------------------------------
% espacos_puzzle(Puzzle, Espacos)
% Puzzle e um puzzle. Espacos e a lista de espacos de Puzzle.
%
% espacos_puzzle_aux(Puzzle, Espacos, Lst_Esp_Aux, H_V)
% Predicado auxiliar para espacos_puzzle, que retorna todos os
% Espacos das colunas e linhas de um Puzzle.
% -------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
    espacos_puzzle_aux(Puzzle, Espacos_h, [], h),
    mat_transposta(Puzzle, Puz_Tr),
    espacos_puzzle_aux(Puz_Tr, Espacos_v, [], v),
    append(Espacos_h, Espacos_v, Espacos).

espacos_puzzle_aux([], Espacos, Espacos, _).

espacos_puzzle_aux([Fila | Resto], Espacos, Lst_Esp_Aux, H_V) :-
    espacos_fila(H_V, Fila, Esps),
    append(Lst_Esp_Aux, Esps, Lst_Esp),
    espacos_puzzle_aux(Resto, Espacos, Lst_Esp, H_V).

% -------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos e uma lista de espacos e Esp e um espaco. Esps_com e a
% lista de espacos com variaveis em comum com Esp, excetuando Esp.
%
% espacos_com_posicoes_comuns_aux(Espacos, Esp, Esps)
% Predicado auxiliar para espacos_com_posicoes_comuns com as
% condicoes necessarias para o bagof. 
% -------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(Esps, 
    espacos_com_posicoes_comuns_aux(Espacos, Esp, Esps), 
    Esps_com).

espacos_com_posicoes_comuns_aux(Espacos, Esp, Esps) :-
    get_Lst(Esp, Lst_Esp),
    member(Esps, Espacos),
    get_Lst(Esps, Lst_Esps),
    member(Var, Lst_Esp),
    member(Var2, Lst_Esps),
    Var == Var2,
    Esp \== Esps.

% -------------------------------------------------------------------
% permutacoes_soma_espacos(Espacos, Perms_soma)
% Espacos e uma lista de espacos. Perms_soma e a lista de listas de 2
% elementos, em que o 1o elemento e um espaco de Espacos e o 2o e a
% lista ordenada de permutacoes cuja soma e igual a soma do espaco.
%
% permutacoes_soma_espacos_aux(Espacos, Esp, Perms)
% Predicado auxiliar para permutacoes_soma_espacos com as condicoes
% necessarias para o bagof.
% -------------------------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    bagof([Esp, Perms], 
    permutacoes_soma_espacos_aux(Espacos, Esp, Perms), 
    Perms_soma).
permutacoes_soma_espacos_aux(Espacos, Esp, Perms) :-
    member(Esp, Espacos),
    get_S(Esp, S),
    get_Lst(Esp, Lst),
    length(Lst, Tam),
    permutacoes_soma(Tam, [1, 2, 3, 4, 5, 6, 7, 8, 9], S, Perms).

% -------------------------------------------------------------------
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de
% espacos, e Perms_soma e uma lista de listas obtida pelo predicado
% anterior. Perm e uma permutacao possivel para o espaco Esp
%
% permutacao_espaco(Esp, Perms_soma, Perm_Esp)
% Predicado auxiliar para permutacao_possivel_espaco que obtem as
% permutacoes de Esp.
%
% permutacao_espaco_comum(Lst_Esp_Com, Perms_soma, Lst)
% Predicado auxiliar para permutacao_possivel_espaco que obtem a
% lista de permutacoes dos espacos em comum com Esp.
%
% permutacao_possivel_espaco_aux(Esp_Lst, Perm_Esp_Comum, Res, Lst)
% Predicado auxiliar para permutacao_possivel_espaco que obtem a
% permutacao Perm possivel para Esp, recursivamente, verificando se
% cada elemento da permutacao de Esp esta contido em pelo menos uma
% das permutacoes dos seus espacos em comum.
% -------------------------------------------------------------------
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    % arranjar permutacoes de Esp
    permutacao_espaco(Esp, Perms_soma, Perm_Esp),
    % espacos que tem posicoes em comum com Esp
    espacos_com_posicoes_comuns(Espacos, Esp, Esp_Com),
    Esp_Com \== [],
    !,
    % lista das permutacoes dos espacos em comum
    bagof(Lst, 
    permutacao_espaco_comum(Esp_Com, Perms_soma, Lst), 
    Perm_Esp_Comum),
    member(Esp_Lst, Perm_Esp),
    permutacao_possivel_espaco_aux(Esp_Lst, Perm_Esp_Comum, Perm, []).

permutacao_possivel_espaco(Perm, Esp, _, Perms_soma) :-
    % se Esp nao tiver espacos em comum, entao Perm sera igual as
    % permutacoes de Esp
    permutacao_espaco(Esp, Perms_soma, Perm_Esp),
    Perm = Perm_Esp.

permutacao_espaco_comum(Lst_Esp_Com, Perms_soma, Lst) :-
    member(Perms, Perms_soma), 
    member(Esp, Perms), 
    member(Esp_Com, Lst_Esp_Com), 
    Esp == Esp_Com,
    nextto(Esp, Lst, Perms).

permutacao_espaco(Esp, Perms_soma, Perm_Esp) :-
    bagof(Perms,
    (member(Perms, Perms_soma), member(Esp_Perms, Perms), Esp_Perms == Esp),
    Lst_Perms),
    append(Lst_Perms, Lst),
    nth0(1, Lst, Perm_Esp).

permutacao_possivel_espaco_aux(_, [], Res, Res).

permutacao_possivel_espaco_aux([Num | Resto], [Perms | Resto2], Res, Lst) :-
    (member(Lst_Perms, Perms),
    member(Num, Lst_Perms)),
    append(Lst, [Num], Lst1),
    !,
    permutacao_possivel_espaco_aux(Resto, Resto2, Res, Lst1).

% -------------------------------------------------------------------
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Espacos e uma lista de espacos, Perms_soma e uma lista de listas
% obtida pelo predicado permutacoes_soma_espacos e Esp e um espaco.
% Perms_poss e uma lista de 2 elementos em que o 1o e a lista de
% variaveis de Esp e o segundo e a lista ordenada de permutacoes
% possiveis para o espaco Esp.
% -------------------------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    get_Lst(Esp, Lst),
    append([], [Lst], Res),
    bagof(Perm,
    permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma),
    Perm_Desordenado),
    sort(Perm_Desordenado, Perm_Ordenado),
    append(Res, [Perm_Ordenado], Perms_poss).

% -------------------------------------------------------------------
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos e uma lista de espacos. Perms_poss_esps e a lista de
% permutacoes possiveis.
% 
% permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, Perms_poss)
% Predicado auxiliar para permutacoes_possiveis_espacos com as
% condicoes necessarias para o bagof.
% -------------------------------------------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss,
    permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, Perms_poss),
    Perms_poss_esps).

permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, Perms_poss) :-
    member(Esp, Espacos), 
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss).

% -------------------------------------------------------------------
% numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms e uma lista de permutacoes. Numeros_comuns e uma lista de
% pares (pos, num), significando que todas as listas de Lst_Perms
% contem o numero num na  posicao pos.
%
% numeros_comuns_aux(Lst, Pos, Numeros_comuns, Lst_aux)
% Predicado auxiliar para numeros_comuns que verifica, recursivamente
% se existem numeros comuns entre a Lst_Perms transposta.
% -------------------------------------------------------------------
numeros_comuns(Lst_Perms, Numeros_comuns) :-
    % a matriz transporta da Lst_Perms, coloca numa lista todos os
    % elementos que estao numa determinada posicao de todas as Perms
    mat_transposta(Lst_Perms, Transp_Lst),
    numeros_comuns_aux(Transp_Lst, 1, Numeros_comuns, []).

numeros_comuns_aux([], _, Numeros_comuns, Numeros_comuns).

numeros_comuns_aux([Lst | Resto], Pos, Numeros_comuns, Lst_aux) :-
    % se uma lista for igual a si mesma inversa, entao significa
    % que todos os elementos da lista sao iguais e entao estamos
    % perante um par
    reverse(Lst, Lst_Invert),
    Lst = Lst_Invert,
    !,
    nth0(0, Lst, Num),
    append(Lst_aux, [(Pos, Num)], Res),
    Pos_1 is Pos + 1,
    numeros_comuns_aux(Resto, Pos_1, Numeros_comuns, Res).

numeros_comuns_aux([_ | Resto], Pos, Numeros_comuns, Res) :-
    % se a lista nao for igual a si propria invertida, a recursao
    % continua sem adicionar nenhum novo par
    Pos_1 is Pos + 1,
    numeros_comuns_aux(Resto, Pos_1, Numeros_comuns, Res).

% -------------------------------------------------------------------
% atribui_comuns(Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, acualiza esta
% lista atribuindo a cada espaco numeros comuns a todas as
% permutacoes possiveis para esse espaco.
%
% altera_comuns(Esp, Lst) :-
% Predicado auxiliar para atribui_comuns que altera as posicoes em
% comum com os respetivos valores.
% -------------------------------------------------------------------
atribui_comuns(Perms_Possiveis) :-
    maplist(atribui_comuns_aux, Perms_Possiveis).

atribui_comuns_aux(Lst) :-
    nth0(0, Lst, Esp),
    nth0(1, Lst, Perms),
    numeros_comuns(Perms, Numeros_comuns),
    % se houverem numeros em comum atualiza-se o Esp, alterando as
    % suas posicoes em comum pelo respetivo numero
    Numeros_comuns \== [],
    !,
    maplist(altera_comuns(Esp), Numeros_comuns).

atribui_comuns_aux(_) :-
    % se nao houver nenhum numero em comum nada se altera na
    % Perms_Possiveis
    !.

altera_comuns(Esp, Lst) :-
    Lst = (Pos, Num),
    nth1(Pos, Esp, Var),
    Var = Num.

% -------------------------------------------------------------------
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis. 
% Novas_Perms_Possiveis e  o resultado de tirar permutacoes
% impossiveis de Perms_Possiveis.
% 
% retira_impossiveis_aux(Perms_Possiveis, Esp, Lst_Perm)
% Predicado auxiliar para retira_impossiveis que verifica,
% recursivamente quais as Perms que unificam com o Esp alterado,
% removendo as que nao o unificam.
% -------------------------------------------------------------------
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    retira_impossiveis_aux(Perms_Possiveis, [], Novas_Perms_Possiveis).

retira_impossiveis_aux([], Novas_Perms_Possiveis, Novas_Perms_Possiveis).
retira_impossiveis_aux([Perms | Resto], Lst, Novas_Perms_Possiveis) :-
    nth0(0, Perms, Esp),
    nth0(1, Perms, Lst_Perm),
    % Encontrar todas as Perms que unificam com o espaco, eliminando
    % assim as que nao unificam
    findall(Perm, (member(Perm, Lst_Perm), Esp = Perm), Lst_Perm_At),
    Esp_At = [Esp, Lst_Perm_At],
    append(Lst, [Esp_At], Res),
    retira_impossiveis_aux(Resto, Res, Novas_Perms_Possiveis).
    
% -------------------------------------------------------------------
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis. 
% Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis.
% -------------------------------------------------------------------
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms),
    % Continua a simplificar as Perms_Possiveis enquanto forem
    % diferentes das Novas_Perms
    Perms_Possiveis \== Novas_Perms,
    !,
    simplifica(Novas_Perms, Novas_Perms_Possiveis).

simplifica(Novas_Perms, Novas_Perms_Possiveis) :-
    % Se Perms_Possiveis e Novas_Perms forem finalmente iguais entao
    % unifica-se com Novas_Perms_Possiveis e o ciclo acaba.
    Novas_Perms_Possiveis = Novas_Perms.

% -------------------------------------------------------------------
% inicializa(Puzzle, Perms_Possiveis)
% Puzzle e um puzzle. Perms_Possiveis e a lista de permutacoes 
% possiveis simplificada para Puzzle.
% -------------------------------------------------------------------
inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_Nao_Simplificadas),
    simplifica(Perms_Possiveis_Nao_Simplificadas, Perms_Possiveis).

%          ================================================
%          = Resolucao de listas de permutacoes possiveis =
%          ================================================
% -------------------------------------------------------------------
% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa que
% Escolha e o elemento de Perms_Possiveis.
%
% escolhe_menos_alternativas_aux(Perms_Possiveis, Escolha, Tam, Perm)
% Predicado auxiliar para escolhe_menos_alternativas_aux que,
% recursivamente, e retorna a escolha com menos permutacoes.
% -------------------------------------------------------------------
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    % verifica se em todos os espacos em Perms_Possiveis estao associadas
    % listas de permutacoes unitarias.
    findall(Perm, 
    (member(Perm, Perms_Possiveis), nth0(1, Perm, Lst), 
    length(Lst, Tam), Tam \== 1), X),
    X \== [],
    !,
    % escolhi um numero arbitrariamente grande
    escolhe_menos_alternativas_aux(Perms_Possiveis, Escolha, 10000, _).

escolhe_menos_alternativas_aux([], Escolha, _, Escolha).

escolhe_menos_alternativas_aux([Perm | Resto], Escolha, Max, _) :-
    nth0(1, Perm, Lst),
    length(Lst, Tam),
    Tam \== 1,
    % se o tamanho for o menor, altera-se o Perm
    Tam < Max,
    !,
    escolhe_menos_alternativas_aux(Resto, Escolha, Tam, Perm).

escolhe_menos_alternativas_aux([_ | Resto], Escolha, Max, Perm) :-
    escolhe_menos_alternativas_aux(Resto, Escolha, Max, Perm).

% -------------------------------------------------------------------
% experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, e Escolha e
% um dos seus elementos.
% -------------------------------------------------------------------
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    nth0(0, Escolha, Esp),
    nth0(1, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    select(Escolha, Perms_Possiveis, [Esp, [Perm]], Novas_Perms_Possiveis).

% -------------------------------------------------------------------
% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, 
% Novas_Perms_Possiveis e o resultado de aplicar um algoritmo. 
% -------------------------------------------------------------------

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    % Se escolhe_menos_alternativas retornar False, o que significa
    % que as listas de permutacoes de todos os espacos sao unitarias,
    % o ciclo acaba
    !,
    experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms),
    simplifica(Novas_Perms, Novas_Perms_Possiveis_1),
    resolve_aux(Novas_Perms_Possiveis_1, Novas_Perms_Possiveis).

resolve_aux(Novas_Perms_Possiveis, Novas_Perms_Possiveis).


%          ================================================
%          =             Resolucao de puzzles             =
%          ================================================
% -------------------------------------------------------------------
% resolve(Puz)
% Puz e um puzzle. Este predicado resolve esse puzzle, apos a 
% sua invocacao a grelha de Puz tem todas as variaveis substituidas
% por numeros que respeitam as restricoes de Puz.
% -------------------------------------------------------------------
resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).
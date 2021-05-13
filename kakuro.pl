% Goncalo Mateus, 99225
% working_directory(_, 'c:/Users/gonca/OneDrive/Ambiente de Trabalho/uni/LP/Projeto/Project.LP_20-21').

:- [codigo_comum].

% KAKURO SOLVER

% ================================================
% =                Inicializacao                 =
% ================================================
% combinacoes_soma(N, Els, Soma, Combs)

combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, combinacao(N, Els, Comb), L_Comb),
    % encontra todas as combinacoes que sao membros da lista de combinacoes
    % cuja soma dos elementos seja igual a Soma.
    findall(Comb, 
        (member(Comb, L_Comb), sum_list(Comb, Sum), Sum == Soma),
        Combs).

% permutacoes_soma(N, Els, Soma, Perms)
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm,
        (member(Comb, Combs), permutation(Comb, Perm)),
        Unsorted_Perms),
    sort(Unsorted_Perms, Perms).

getS(H_V, P, S) :- H_V \== h -> nth0(0, P, S); nth0(1, P, S).

% estrutura espaco
cria_espaco(S, Lst, espaco(S, Lst)).
get_Lst(espaco(_, Lst), Lst).
get_S(espaco(S, _), S).
change_lst(Lst, espaco(S, _), espaco(S, Lst)).

espaco_fila_aux([], Esp, Esp, _, _).

espaco_fila_aux([P | Q], Esp, Lst_Esp, _, H_V) :-
    is_list(P),
    !,
    getS(H_V, P, S),
    cria_espaco(S, [], Esp_aux),
    espaco_fila_aux(Q, Esp, [Esp_aux | Lst_Esp], [], H_V).

espaco_fila_aux([P | Q], Esp, [Esp_at | Resto], Lst, H_V) :-
    append(Lst, [P], Lst_aux),
    change_lst(Lst_aux, Esp_at, Esp_no),
    espaco_fila_aux(Q, Esp, [Esp_no | Resto], Lst_aux, H_V).

% espaco_fila(Fila, Esp, H_V)
espaco_fila(Fila, Esp, H_V) :-
    espaco_fila_aux(Fila, Esp_aux, [], [], H_V),
    reverse(Esp_aux, Esp_list),
    bagof(Esp, 
    (member(Esp, Esp_list), get_Lst(Esp, Lst), Lst \== []),
    Esp_lst),
    member(Esp, Esp_lst).

% espacos_fila(H_V, Fila, Espacos)
espacos_fila(_, Fila, Espacos) :-
    exclude(is_list, Fila, Lst),
    Lst = [],
    !,
    Espacos = Lst.

espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

% espacos_puzzle(Puzzle, Espacos)
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

% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(Esps, 
    espacos_com_posicoes_comuns_aux(Espacos, Esp, Esps), 
    Esps_com).

espacos_com_posicoes_comuns_aux(Espacos, Esp, Esps) :-
    get_Lst(Esp, Lst),
    member(Esps, Espacos),
    get_Lst(Esps, Lst1),
    member(X, Lst),
    member(X2, Lst1),
    X == X2,
    Esp \== Esps.

% permutacoes_soma_espacos(Espacos, Perms_soma)
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    bagof([Esp, Perms], 
    permutacoes_soma_espacos_aux(Espacos, Esp, Perms), 
    Perms_soma).
permutacoes_soma_espacos_aux(Espacos, Esp, Perms) :-
    member(Esp, Espacos),
    get_S(Esp, S),
    get_Lst(Esp, Lst),
    length(Lst, Len),
    permutacoes_soma(Len, [1, 2, 3, 4, 5, 6, 7, 8, 9], S, Perms).

% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    % arranjar permutacoes para Esp
    permutacao_espaco(Esp, Perms_soma, Perm_Esp),
    % espacos que tem posicoes em comum com Esp
    espacos_com_posicoes_comuns(Espacos, Esp, Esp_Com),
    % lista das permutacoes dos espacos em comum
    bagof(Lst, permutacao_espaco_comum(Esp_Com, Perms_soma, Lst), Perm_Esp_Comum),
    member(Esp_Lst, Perm_Esp),
    recursao(Esp_Lst, Perm_Esp_Comum, Res, []),
    Res == Esp_Lst,
    Perm = Res.

permutacao_espaco_comum(Esp_Com, Perms_soma, Lst) :-
    member(X, Perms_soma), 
    member(X2, X), 
    member(X3, Esp_Com), 
    X2 == X3,
    nextto(X2, Lst, X).

permutacao_espaco(Esp, Perms_soma, Perm_Esp) :-
    bagof(Y, (member(Y, Perms_soma), member(Y2, Y), Y2 == Esp), Lst1),
    append(Lst1, Lst),
    nth0(1, Lst, Perm_Esp).

recursao([], [], Res, Res).

recursao([Num | Resto], [Perms | Resto2], Res, Lst) :-
    (member(Lst_Perms, Perms),
    member(Num, Lst_Perms)),
    append(Lst, [Num], Lst1),
    !,
    recursao(Resto, Resto2, Res, Lst1).

% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    get_Lst(Esp, Lst),
    append([], [Lst], Res),
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Unsorted_Perm),
    sort(Unsorted_Perm, Sorted_Perm),
    append(Res, [Sorted_Perm], Perms_poss).

% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss,permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, Perms_poss), Perms_poss_esps).

permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, Perms_poss) :-
    member(Esp, Espacos), 
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss).

% numeros_comuns(Lst_Perms, Numeros_comuns
% atribui_comuns(Perms_Possiveis)
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% inicializa(Puzzle, Perms_Possiveis)

% ================================================
% = Resolucao de listas de permutacoes possiveis =
% ================================================
% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis)
% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)

% ================================================
% =             Resolucao de puzzles             =
% ================================================
% resolve(Puz)
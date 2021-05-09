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

% estrutura espaco
espaco_fila_aux_h([], Esp, Esp, _).

espaco_fila_aux_h([P | Q], Esp, Lst_Esp, _) :-
    is_list(P),
    !,
    nth0(1, P, S),
    cria_espaco(S, [], Esp_aux),
    espaco_fila_aux_h(Q, Esp, [Esp_aux | Lst_Esp], []).

espaco_fila_aux_h([P | Q], Esp, [Esp_at | Resto], Lst) :-
    append(Lst, [P], Lst_aux),
    change_lst(Lst_aux, Esp_at, Esp_no),
    espaco_fila_aux_h(Q, Esp, [Esp_no | Resto], Lst_aux).

espaco_fila(Fila, Esp, H_V) :-
    H_V = h,
    !,
    espaco_fila_aux_h(Fila, Esp_aux, [], []),
    reverse(Esp_aux, Esp_list),
    bagof(Esp, (member(Esp, Esp_list), get_Lst(Esp, Lst), Lst \== []), Esp_lst),
    member(Esp, Esp_lst).

espaco_fila(Fila, Esp, _) :-
    espaco_fila_aux_v(Fila, Esp_aux, [], []),
    reverse(Esp_aux, Esp_list),
    bagof(Esps, (member(Esps, Esp_list), get_Lst(Esps, Lst), Lst \== []), Esp_lst),
    member(Esp, Esp_lst).

espaco_fila_aux_v([], Esp, Esp, _).

espaco_fila_aux_v([P | Q], Esp, Lst_Esp, _) :-
    is_list(P),
    !,
    nth0(0, P, S),
    cria_espaco(S, [], Esp_aux),
    espaco_fila_aux_v(Q, Esp, [Esp_aux | Lst_Esp], []).

espaco_fila_aux_v([P | Q], Esp, [Esp_at | Resto], Lst) :-
    append(Lst, [P], Lst_aux),
    change_lst(Lst_aux, Esp_at, Esp_no),
    espaco_fila_aux_v(Q, Esp, [Esp_no | Resto], Lst_aux).

cria_espaco(S, Lst, espaco(S, Lst)).
get_Lst(espaco(_, Lst), Lst).
change_lst(Lst, espaco(S, _), espaco(S, Lst)).

% espacos_fila(H_V, Fila, Espacos)
espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).
% espacos_puzzle(Puzzle, Espacos)
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% permutacoes_soma_espacos(Espacos, Perms_soma)
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% numeros_comuns(Lst_Perms, Numeros_comuns)
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
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

espaco_fila(Fila, Esp, H_V) :-
    espaco_fila_aux(Fila, Esp_aux, [], [], H_V),
    reverse(Esp_aux, Esp_list),
    bagof(Esp, (member(Esp, Esp_list), get_Lst(Esp, Lst), Lst \== []), Esp_lst),
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
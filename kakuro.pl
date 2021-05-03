% Gonçalo Mateus, 99225
% working_directory(_, 'c:/Users/gonca/OneDrive/Ambiente de Trabalho/uni/LP/Projeto').

:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, combinacao(N, Els, Comb), L_Comb),
    % encontra todas as combinações que são membros da lista de combinações
    % cuja soma dos elementos seja igual a Soma.
    findall(Comb, (member(Comb, L_Comb), sum_list(Comb, Sum), Sum == Soma), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(Comb, Combs), permutation(Comb, Perm)), Unsorted_Perms),
    sort(Unsorted_Perms, Perms).

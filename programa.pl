%%%%%%%%%%%%%%%%%%%%%%%%%% Influencers %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% Punto 1

usuario(ana, youtube, 3000000).
usuario(ana, instagram, 2700000).
usuario(ana, tiktok, 1000000).
usuario(ana, twitch, 2).

usuario(beto, twitch, 120000).
usuario(beto, youtube, 6000000).
usuario(beto, instagram, 1100000).

usuario(cami, tiktok, 2000).
usuario(dani, youtube, 1000000).
usuario(evelyn, instagram, 1).

%%%%%%%%%%%%%%%%% Punto 2

% Parte A

influencer(Usuario):-
    usuario(Usuario, _,_),
    findall(Seguidores, usuario(Usuario, _, Seguidores), CantSeguidores),
    sum_list(CantSeguidores, SeguidoresTotal),
    SeguidoresTotal > 10000.

% Parte B

omnipresente(Influencer):-
    influencer(Influencer),
    forall(usuario(_, Red, _), usuario(Influencer, Red, _)).

% Parte C

exclusivo(Influencer):-
    influencer(Influencer),
    usuario(Influencer, Red, _),
    not((usuario(Influencer, Red2, _), Red2 \= Red)).

%%%%%%%%%%%%%%%%% Punto 3

% Parte A

post(ana, tiktok, video(1, [evelyn, beto])).
post(ana, tiktok, video(1, [ana])).
post(ana, instagram, foto([ana])).

post(beto, instagram, foto([])).
post(cami, twitch, stream(leagueOfLegends)).
post(cami, youtube, video(5, [cami])).

post(evelyn, instagram, foto([evelyn, cami])).

% Parte B

tematica(juego, leagueOfLegends).
tematica(juego, minecraft).
tematica(juego, aoe).

%%%%%%%%%%%%%%%%% Punto 4

adictiva(Red):-
    post(_, Red, _),
    forall(post(_, Red, Posteo), contenidoAdictivo(Posteo)).

contenidoAdictivo(video(Minutos, _)):-
    Minutos < 3.

contenidoAdictivo(stream(Tematica)):-
    tematica(juego, Tematica).

contenidoAdictivo(foto(Participantes)):-
    length(Participantes, CantParticipantes),
    CantParticipantes < 4.

%%%%%%%%%%%%%%%%% Punto 5 
colaboran(U1, U2):- 
    colaboracion(U1,U2), 
    U1 \= U2.
    
colaboran(U1,U2):- 
    colaboracion(U2,U1),
    U1 \= U2.

colaboracion(U1,U2):-
    post(U1, _, Posteo), 
    postColaborativo(U1, Posteo, U2). 

postColaborativo(U1, Posteo, U2):-
    quienAparece(U1, Posteo, Participantes),
    member(U2, Participantes).

quienAparece(_, video(_, Participantes), Participantes).
quienAparece(_, foto(Participantes), Participantes).
quienAparece(Autor, stream(_), [Autor]). 

%%%%%%%%%%%%%%%%% Punto 6

caminoALaFama(Usuario):-
    not(influencer(Usuario)),
    tieneFama(Famoso),
    colaboracion(Famoso, Usuario),
    Famoso \= Usuario.

tieneFama(Usuario):-
    influencer(Usuario).
tieneFama(Usuario):-
    caminoALaFama(Usuario).

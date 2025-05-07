:-consult(league_data). 

%helper predicates

myappend([], L, L).
myappend([H|T], L2, [H|NT]) :-
    myappend(T, L2, NT).

member(X, [X|_]).
member(X, [_|Tail]):-
member(X, Tail).

length([],0).
length([_|Tail], N):-
length(Tail, TmpN),
 N is TmpN+1.


%Task1

players_in_team(Team, Players) :-
    player_bucket(Team, [], Players),!.

player_bucket(Team, PlayersAcc, Players) :-
    player(X, Team, _),
    \+ member(X, PlayersAcc),  
    myappend(PlayersAcc, [X], NewTemp),
    player_bucket(Team, NewTemp, Players).
	
	
player_bucket(_, Players, Players).



%Task2

team_count_by_country(Country, Count) :-  
    teams_bucket(Country, [], Teams),  
    length(Teams, Count),!.

teams_bucket(Country, TeamsAcc, Teams) :-  
    team(Name, Country, _),  
    \+ member(Name, TeamsAcc),  
    myappend(TeamsAcc, [Name], NewAcc),  
    teams_bucket(Country, NewAcc, Teams).  

teams_bucket(_, Teams, Teams).  


%Task3

most_successful_team(Team) :-
    team(FirstTeam, _, FirstTitles),  
    max_team_titles(FirstTeam, FirstTitles, Team),!.

max_team_titles(CurrentMaxTeam, CurrentMaxTitles, Team) :-  
    team(Name, _, Titles),  
    Titles > CurrentMaxTitles, 
    max_team_titles(Name, Titles, Team).  

max_team_titles(Team, _, Team).


%task4

matches_of_team(Team, Matches) :-
    matches_played_bucket(Team, [], Matches),!.

matches_played_bucket(Team,MatchesPlayedAcc,Matches):-
	match(Team,Team2,TeamGoals,Team2Goals),
	\+ member((Team,Team2,Team1Goals,Team2Goals),MatchesPlayedAcc),
	myappend(MatchesPlayedAcc,[(Team,Team2,TeamGoals,Team2Goals)],TmpMatchesPlayedAcc),
	matches_played_bucket(Team,TmpMatchesPlayedAcc,Matches).
	
matches_played_bucket(Team,MatchesPlayedAcc,Matches):-
	match(Team2,Team,Team2Goals,Team1Goals),
	\+ member((Team2,Team,Team2Goals,Team1Goals),MatchesPlayedAcc),
	myappend(MatchesPlayedAcc,[(Team2,Team,Team2Goals,Team1Goals)],TmpMatchesPlayedAcc),
	matches_played_bucket(Team,TmpMatchesPlayedAcc,Matches).


matches_played_bucket(_, Matches, Matches).  



% Task5
num_matches_of_team(Team, Count) :-
	matches_of_team(Team, Matches),
	length(Matches,Count).


%Task6

top_scorer(P):-
	goals(Player, PlayerNumOfGoals),
	maximam_goals_achieved(Player,PlayerNumOfGoals,P),!.
	
maximam_goals_achieved(CurrentMaxPlayer,CurrentMaxGoals,Player):-
	goals(PlayerName,PlayerNumOfGoals),
	PlayerNumOfGoals > CurrentMaxGoals,
	maximam_goals_achieved(PlayerName,PlayerNumOfGoals,Player).
	
maximam_goals_achieved(CurrentMaxPlayer,_,CurrentMaxPlayer).

	
%Task7

players_positions_in_team(Team, Positions) :-
    positions_bucket(Team, [], Positions).

positions_bucket(Team, Acc, Positions) :-
    player(_, Team, Pos),
    \+ member(Pos, Acc),
    myappend(Acc, [Pos], NewAcc),
    positions_bucket(Team, NewAcc, Positions).
positions_bucket(_, Positions, Positions).

count_occurrences(_, [], 0).
count_occurrences(Pos, [Pos|Rest], Count) :-
    count_occurrences(Pos, Rest, SubCount),
    Count is SubCount + 1.
count_occurrences(Pos, [Other|Rest], Count) :-
    Pos \= Other,
    count_occurrences(Pos, Rest, Count).

count_positions([], []).
count_positions([Pos|Rest], [(Pos, Count)|CountedRest]) :-
    count_occurrences(Pos, [Pos|Rest], Count),
    remove_all(Pos, Rest, Remaining),
    count_positions(Remaining, CountedRest).

remove_all(_, [], []).
remove_all(Pos, [Pos|Rest], Remaining) :-
    remove_all(Pos, Rest, Remaining).
remove_all(Pos, [Other|Rest], [Other|Remaining]) :-
    Pos \= Other,
    remove_all(Pos, Rest, Remaining).

find_max_position([(Pos, Count)], Pos).
find_max_position([(Pos1, Count1), (Pos2, Count2)|Rest], MaxPos) :-
    compare_counts((Pos1, Count1), (Pos2, Count2), Winner),
    find_max_position([Winner|Rest], MaxPos).

compare_counts((Pos1, Count1), (Pos2, Count2), (Pos1, Count1)) :-
    Count1 >= Count2.
compare_counts((Pos1, Count1), (Pos2, Count2), (Pos2, Count2)) :-
    Count2 > Count1.

most_common_position_in_team(Team, Pos) :-
    players_positions_in_team(Team, Positions),
    Positions \= [],
    count_positions(Positions, Counted),
    find_max_position(Counted, Pos), !.
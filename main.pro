% name surname
% id
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.


% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance) :-
    Distance is abs(Agent1.x - Agent2.x) + abs(Agent1.y - Agent2.y).

% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents(State, NumberOfAgents) :-
    State = [Agents, _, _, _],
    dict_pairs(Agents, _, Pairs),
    find_length(Pairs, NumberOfAgents).

% Function for finding the length of a list
find_length([], 0).
find_length([_|T], Length) :-
    find_length(T, Accumulator),
    Length is Accumulator + 1.

% 3- value_of_farm(+State, -Value)
value_of_farm(State, Value) :-
    State = [Agents, Objects, _, _],
    dict_pairs(Agents, _, Pairs1),
    dict_pairs(Objects, _, Pairs2),
    get_value(Pairs1, Value1),
    get_value(Pairs2, Value2),
    Value is Value1 + Value2.

% Function for calculating values from a given list of Key-Value pairs
get_value([], 0).
get_value(_-V, Value) :- V.subtype = wolf, Value is 0.
get_value(_-V, Value) :- value(V.subtype, Value). % Get value from a single Key-Value pair where Value is either a single agent or a single object dictionary
get_value([H|T], Value) :-
    get_value(H, Head_Value),
    get_value(T, Tail_Value),
    Value is Head_Value + Tail_Value.

% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
find_food_coordinates(State, AgentId, Coordinates) :-
    State = [Agents, Objects, _, _],
    get_agent(State, AgentId, Agent),  % Get the Agent object we want to find food locations for
    (
        (Agent.type = carnivore -> % If Agent is a wolf
            findall( % Record all the possible locations inside 'Coordinates'
                Coordinate,
                (
                    can_eat(Agent.subtype, Food),  % Get the subtype of the foods the wolf Agent can eat (cow or chicken)
                    get_agent_location_from_subtype(Agents, Food, Coordinate)  % Get the [x,y] location of the Food
                ),
                Coordinates
            )
        );

        (Agent.type = herbivore -> 
            findall(
                Coordinate,
                (
                    can_eat(Agent.subtype, Food),  % Get the subtype of the foods the cow/chicken Agent can eat
                    get_object_location_from_subtype(Objects, Food, Coordinate)
                ),
                Coordinates
            )
        )
    ).

% Function to find locations of agents with subtype 'Agent_Subtype'
get_agent_location_from_subtype(Agents, Agent_Subtype, [X, Y]) :-
    get_dict(_, Agents, Agent),  % Check all agents
    get_dict(subtype, Agent, Agent_Subtype),  % Check if the subtype of the agent we are currently looking at matches Agent_Subtype
    get_dict(x, Agent, X),  % Get the x coordinate of the current Agent
    get_dict(y, Agent, Y).  % Get the y coordinate of the current Agent

% Function to find locations of objects with subtype 'Object_Subtype'
get_object_location_from_subtype(Objects, Object_Subtype, [X, Y]) :-
    get_dict(_, Objects, Object),
    get_dict(subtype, Object, Object_Subtype),
    get_dict(x, Object, X),
    get_dict(y, Object, Y).


% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
%find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-


% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)



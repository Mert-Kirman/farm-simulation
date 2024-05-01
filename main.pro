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
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
    State = [Agents, _, _, _],
    get_agent(State, AgentId, Agent1),  % Our Agent
    get_dict(CurId, Agents, Agent2),  % Choose a candidate for the nearest agent, suppose Agent2 is the closest agent for now
    Agent1 \= Agent2,  % Make sure Agent2 is not the same as Agent1
    agents_distance(Agent1, Agent2, CurMinDistance),
    dict_pairs(Agents, _, Pairs),
    find_nearest_agent_Id(Agent1, Pairs, CurMinDistance, _, CurId, Id),  % Find the Id of the agent that is closest to our Agent1
    NearestAgent = Agents.Id,
    Coordinates = [NearestAgent.x, NearestAgent.y],
    !.

% Helper function to keep track of the nearest agent with the minimum distance to a given Agent
find_nearest_agent_Id(_, [], CurMinDistance, CurMinDistance, CurMinId, CurMinId).

find_nearest_agent_Id(Agent1, [K-V|T], CurMinDistance, MinDistance, CurMinId, MinId) :-
    % If agents are same, pass
    (
        Agent1 = V,
        find_nearest_agent_Id(Agent1, T, CurMinDistance, MinDistance, CurMinId, MinId)
    );
    
    % If agents are different, check if a new minimum distance can be found
    agents_distance(Agent1, V, CurrentDistance),
    CurrentDistance < CurMinDistance,  % A new minimum distance has been found
    find_nearest_agent_Id(Agent1, T, CurrentDistance, MinDistance, K, MinId).

find_nearest_agent_Id(Agent1, [_-V|T], CurMinDistance, MinDistance, CurMinId, MinId) :-
    agents_distance(Agent1, V, CurrentDistance),
    CurrentDistance >= CurMinDistance,  % Recorded minimum distance has not been changed
    find_nearest_agent_Id(Agent1, T, CurMinDistance, MinDistance, CurMinId, MinId).


% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :-
    State = [Agents, Objects, _, _],
    get_agent(State, AgentId, Agent1),  % Get the agent we want to find the nearest food for
    (
        (Agent1.type = carnivore -> % If Agent1 is a wolf
            get_dict(CurId, Agents, Agent2), % Food is either cow or chicken, both of which are in the Agents dictionary
            Agent1 \= Agent2,  % Food candidate cannot be itself
            Agent2.subtype \= wolf,  % Food candidate cannot be a wolf
            agents_distance(Agent1, Agent2, CurMinDistance),
            dict_pairs(Agents, _, Pairs)  % Food will be searched in the Agents dictionary
        );

        (Agent1.type = herbivore -> % Agent1 is either a cow or a chicken
            get_dict(CurId, Objects, Object), % Food item is in the Objects dictionary
            can_eat(Agent1.subtype, Object.subtype),
            agents_distance(Agent1, Object, CurMinDistance),
            dict_pairs(Objects, _, Pairs)  % Food will be searched in the Objects dictionary
        )
    ),
    find_nearest_food_Id(Agent1, Pairs, CurMinDistance, Distance, CurId, Id),  % Get the Id of the nearest food item
    (
        (Agent1.type = carnivore ->
            NearestFood = Agents.Id    
        );
        (Agent1.type = herbivore ->
            NearestFood = Objects.Id  
        )
    ),
    Coordinates = [NearestFood.x, NearestFood.y],
    FoodType = NearestFood.subtype,
    !.


% Helper function to keep track of the nearest food item Id with the minimum distance to a given Agent
find_nearest_food_Id(_, [], CurMinDistance, CurMinDistance, CurMinId, CurMinId).

find_nearest_food_Id(Agent1, [K-V|T], CurMinDistance, MinDistance, CurMinId, MinId) :-
    % If agents are same in case agent1 is a wolf or if agent1 does not eat the current food, pass
    (
        (\+ can_eat(Agent1.subtype, V.subtype)),
        find_nearest_food_Id(Agent1, T, CurMinDistance, MinDistance, CurMinId, MinId)
    );
    
    % If agents are different when agent1 is a wolf or agent1 eats the food, check if a new minimum distance can be found
    agents_distance(Agent1, V, CurrentDistance),
    CurrentDistance < CurMinDistance,  % A new minimum distance has been found
    find_nearest_food_Id(Agent1, T, CurrentDistance, MinDistance, K, MinId).

find_nearest_food_Id(Agent1, [_-V|T], CurMinDistance, MinDistance, CurMinId, MinId) :-
    agents_distance(Agent1, V, CurrentDistance),
    CurrentDistance >= CurMinDistance,  % Recorded minimum distance has not been changed
    find_nearest_food_Id(Agent1, T, CurMinDistance, MinDistance, CurMinId, MinId).

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State, AgentId, X, Y, [], _) :-  % Check if location has been reached
    get_agent(State, AgentId, Agent),
    Agent.x = X, Agent.y = Y. % If agent has reached the destination location

move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) :-  % Agent has not reached the location and depth limit not exceeded yet
    DepthLimit > 0,
    get_agent(State, AgentId, Agent),
    can_move(Agent.subtype, Action),  % Get possible directions the agent can move along
    move(State, AgentId, Action, NewState),  % Check if this move can be made (no obstacles or map borders)
    NewDepthLimit is DepthLimit - 1,
    ActionList = [Action|RestOfActionList],
    move_to_coordinate(NewState, AgentId, X, Y, RestOfActionList, NewDepthLimit).

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId,ActionList, DepthLimit) :-
    find_nearest_food(State, AgentId, Coordinates, _, _),  % Find the coordinates of the nearest food
    [X,Y] = Coordinates,
    move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit).

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)
consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, _) :-  % Base case, no more nearest food can be found
    (\+ find_nearest_food(State, AgentId, _, _, _) ) ->
        value_of_farm(State, Value),  % Return total farm value
        NumberOfMoves is 0,
        get_agent(State, AgentId, Agent),
        NumberOfChildren is Agent.children,  % Return children count of our agent
        !.

consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit) :-
    % Find the coordinates of the nearest food that is reachable
    find_nearest_food(State, AgentId, Coordinates, _, _),
    Coordinates = [Target_X,Target_Y],

    % Find the minimum number of moves to reach the food with coordinates [x, y]
    get_agent(State, AgentId, Agent),
    smallest_number_of_moves(State, AgentId, Agent.x, Agent.y, Target_X, Target_Y, MinNumberOfMoves, DepthLimit, [[0, Agent.x, Agent.y]], []),
    
    % Move the agent to the food's location and create the modified state
    State = [Agents, Objects, Time, Turnorder],
    put_dict(x, Agent, Target_X, NewAgent1),
    put_dict(y, NewAgent1, Target_Y, NewAgent2),
    put_dict(AgentId, Agents, NewAgent2, NewAgents),
    NewState = [NewAgents, Objects, Time, Turnorder],

    % Agent eats the food
    eat(NewState, AgentId, NewState2),

    % Continue until the agent can no more eat food
    consume_all(NewState2, AgentId, TmpNumberOfMoves, Value, NumberOfChildren, DepthLimit),

    % Return values requested
    NumberOfMoves is TmpNumberOfMoves + MinNumberOfMoves.
    
% Function to calculate the shortest path to a given coordinate and return amount of moves
smallest_number_of_moves(_, _, Agent_X, Agent_Y, Target_X, Target_Y, MinNumberOfMoves, _, NodesToVisit, _) :-  % Base case
    Agent_X = Target_X, Agent_Y = Target_Y,  % Target location reached
    NodesToVisit = [VisitedNode|_],
    VisitedNode = [Depth|_],
    MinNumberOfMoves = Depth,
    !.

smallest_number_of_moves(State, AgentId, Agent_X, Agent_Y, Target_X, Target_Y, MinNumberOfMoves, DepthLimit, NodesToVisit, VisitedNodes) :-
    % Remove the current node as it is not the target node and add it to the visited nodes list
    NodesToVisit = [VisitedNode|UnvisitedNodes],
    UpdatedVisitedNodes = [VisitedNode|VisitedNodes],

    % Depth limit should not be exceeded
    VisitedNode = [Depth|_],
    Depth < DepthLimit,

    % Create the new state for move predicate to use and find PossibleActions
    State = [Agents, Objects, Time, Turnorder],
    get_agent(State, AgentId, Agent),
    put_dict(x, Agent, Agent_X, NewAgent1),
    put_dict(y, NewAgent1, Agent_Y, NewAgent2),
    put_dict(AgentId, Agents, NewAgent2, NewAgents),
    NewState = [NewAgents, Objects, Time, Turnorder],

    % Gather possible moves(no obstacle or border) inside 'PossibleActions' list, do not involve moves that will result in a previously visited node
    findall(
        Action, 
        (can_move(Agent.subtype, Action), move(NewState, AgentId, Action, TmpNewState), get_agent(TmpNewState, AgentId, TmpAgent), \+member([_, TmpAgent.x, TmpAgent.y], UpdatedVisitedNodes), \+member([_, TmpAgent.x, TmpAgent.y], UnvisitedNodes)),
        PossibleActions
    ),

    % Apply BFS to find shortest path
    find_nodes_to_visit(NewState, AgentId, PossibleActions, NewNodesToVisit, Depth),
    enqueue(NewNodesToVisit, UnvisitedNodes, UpdatedNodesToVisit),

    % Recursion until base case is reached
    UpdatedNodesToVisit = [NextNode|_],  % Get the NextNode to visit
    NextNode = [_, New_Agent_X, New_Agent_Y],
    smallest_number_of_moves(NewState, AgentId, New_Agent_X, New_Agent_Y, Target_X, Target_Y, MinNumberOfMoves, DepthLimit, UpdatedNodesToVisit, UpdatedVisitedNodes).

% Function to append first list to the end of the second list, imitating an enqueue operation
enqueue(List1, [], List1).
enqueue(List1, [H|List2], [H|List3]) :- enqueue(List1, List2, List3).

% Function to find new nodes to visit by using an action list
find_nodes_to_visit(_, _, [], [], _).  % Base case

find_nodes_to_visit(State, AgentId, PossibleActions, NewNodesToVisit, ParentDepth) :-
    State = [Agents, _, _, _],
    PossibleActions = [CurrentAction|T],
    call(CurrentAction, AgentId, Agents, NewAgent),
    NewDepth is ParentDepth + 1,  % These neighbor nodes will have +1 depth compared to their parent node
    NewNodeToVisit = [NewDepth, NewAgent.x, NewAgent.y],  % Initialize the new node with depth of '0'
    find_nodes_to_visit(State, AgentId, T, TmpNewNodesToVisit, ParentDepth),
    NewNodesToVisit = [NewNodeToVisit | TmpNewNodesToVisit].

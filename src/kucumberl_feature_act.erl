%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%-------------------------------------------------------------------
%%% @author Roberto Majadas <roberto.majadas@openshine.com>
%%% @copyright (C) 2012, Roberto Majadas
%%% @doc
%%%
%%% @end
%%% Created : 11 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%%-------------------------------------------------------------------
-module(kucumberl_feature_act).
-include("kucumberl.hrl").

%% API
-export([run/2]).

%%%===================================================================
%%% API
%%%===================================================================

run(F, {setup, ScnID, EID}) ->
    State = lists:foldl(fun(Mod,Acc) ->
                                set_modstate(Acc, Mod, Mod:setup())
                        end,
                        dict:new(),
                        F#feature.fcode#feature_code.setup_mod),

    ets:insert(kctx, {{F#feature.id, state, ScnID, EID}, State}),
    ets:insert(kctx, {{F#feature.id, status, ScnID, EID}, ok}),
    ets:insert(kctx, {{F#feature.id, setup, ScnID, EID}, ok}),
    F;
run(F, {teardown, ScnID, EID}) ->
    [[State0]] = ets:match(kctx, {{F#feature.id, state, ScnID, EID}, '$1'}),
    State = lists:foldr(fun(Mod,Acc) ->
                                ModState = Mod:teardown(get_modstate(Acc,Mod)),
                                set_modstate(Acc, Mod, ModState)
                        end,
                        State0,
                        F#feature.fcode#feature_code.teardown_mod),

    ets:insert(kctx, {{F#feature.id, state,    ScnID, EID}, State}),
    ets:insert(kctx, {{F#feature.id, teardown, ScnID, EID}, ok}),
    F;
run(F, {background, ScnID, EID, ActID}) ->
    kucumberl_log:emit(init_step, {background, ScnID, EID, ActID}),
    F = run_step(F, {background, ScnID, EID, ActID}),
    kucumberl_log:emit(end_step, {background, ScnID, EID, ActID}),
    F;
run(F, {normal, ScnID, EID, ActID}) ->
    kucumberl_log:emit(init_step, {normal, ScnID, EID, ActID}),
    F = run_step(F, {normal, ScnID, EID, ActID}),
    kucumberl_log:emit(end_step, {normal, ScnID, EID, ActID}),
    F;
run(F,_) -> F.


%%%===================================================================
%%% Internal functions
%%%===================================================================

run_step(F, {ScnType, ScnID, EID, ActID}) ->
    [[Status]] = ets:match(kctx, {{F#feature.id, status, ScnID, EID}, '$1'}),
    [[State]] = ets:match(kctx, {{F#feature.id, state, ScnID, EID}, '$1'}),

    Act = case ScnType of
	      background ->
		  lists:nth(ActID,
			    F#feature.background#scenario.actions);
	      normal ->
		  Scn = lists:nth(ScnID, F#feature.scenarios),
		  lists:nth(ActID, Scn#scenario.actions)
	  end,
    Step = case Act#action.step of
	       and_step ->
		   [[LastStep]] = ets:match(kctx, {{F#feature.id, step, ScnID, EID}, '$1'}),
		   LastStep;
	       S ->
		   ets:insert(kctx, {{F#feature.id, step, ScnID, EID}, S}),
		   S
	   end,

    case find_step_handlers(F, ScnID, EID, Step, Act) of
	[] ->
	    %%io:format("F: ~p Scn: ~p E: ~p T: ~p Act: ~p NO HANDLERS~n", [F#feature.id, ScnID, EID, ScnType, ActID]),
	    ets:insert(kctx, {{F#feature.id, status, ScnID, EID}, disabled}),
	    ets:insert(kctx, {{F#feature.id, ScnType, ScnID, EID, ActID}, not_implemented});
	[StepImpl|_] ->
	    case Status of
		ok ->
		    {Mod, Re} = StepImpl,
		    Params = prepare_step_params(F, ScnID, EID, Act, Re),
		    case exec_step(Step, Mod, Re, State, Params) of
			{ok, NewState} ->
			    %%io:format("F: ~p Scn: ~p E: ~p T: ~p Act: ~p OK~n", [F#feature.id, ScnID, EID, ScnType, ActID]),
			    ets:insert(kctx, {{F#feature.id, status, ScnID, EID}, ok}),
			    ets:insert(kctx, {{F#feature.id, state,  ScnID, EID}, NewState}),
			    ets:insert(kctx, {{F#feature.id, ScnType, ScnID, EID, ActID}, ok});
			{failed, Reason} ->
			    %%io:format("F: ~p Scn: ~p E: ~p T: ~p Act: ~p Fail~n", [F#feature.id, ScnID, EID, ScnType, ActID]),
			    ets:insert(kctx, {{F#feature.id, status, ScnID, EID}, failed}),
			    ets:insert(kctx, {{F#feature.id, ScnType, ScnID, EID, ActID}, {failed, Reason}});
			_ ->
			    %%io:format("F: ~p Scn: ~p E: ~p T: ~p Act: ~p Fail~n", [F#feature.id, ScnID, EID, ScnType, ActID]),
			    ets:insert(kctx, {{F#feature.id, status, ScnID, EID}, failed}),
			    ets:insert(kctx, {{F#feature.id, ScnType, ScnID, EID, ActID},
					      {failed, "Wrong value returned"}})
		    end;
		_ ->
		    %%io:format("F: ~p Scn: ~p E: ~p T: ~p Act: ~p Disabled~n", [F#feature.id, ScnID, EID, ScnType, ActID]),
		    ets:insert(kctx, {{F#feature.id, ScnType, ScnID, EID, ActID}, disabled})
	    end
    end,

    F.

find_step_handlers(F, ScnID, EID, Step, Act) ->
    PAct = prepare_act(F, ScnID, EID, Act),

    lists:foldl(
      fun ({Sx, Mod, Re}, Acc) ->
	      case Sx =:= Step of
		  false -> Acc;
		  true ->
		      case re:run(PAct#action.desc, Re) of
			  {match, _} -> Acc ++ [{Mod, Re}] ;
			  _ -> Acc
		      end
	      end
      end,
      [],
      F#feature.fcode#feature_code.steps).

exec_step(Step, Mod, Re, State, Params) ->
    ModState = get_modstate(State, Mod),
    ExecFunc =
	fun () ->
		case Step of
		    given_step -> Mod:given(Re, ModState, Params);
		    when_step  -> Mod:'when'(Re, ModState, Params);
		    then_step  -> Mod:then(Re, ModState, Params)
		end
	end,

    try ExecFunc() of
	{ok,Val} -> {ok,set_modstate(State, Mod, Val)};
        {failed,_}=Res -> Res
    catch
	E:R -> {failed, {E, R, [X || X <- erlang:get_stacktrace(),
                                     begin M=element(1,X),
                                           M /= kucumberl_feature_act andalso
                                           M /= kucumberl_feature_scn andalso
                                           M /= kucumberl_feature
                                     end]}}
    end.

prepare_step_params(F, ScnID, EID, Act, Re) ->
    PAct = prepare_act(F, ScnID, EID, Act),

    P1 = case re:run(PAct#action.desc, Re,
		     [{capture, all_but_first, list}]) of
	     {match, P} -> P;
	     _ -> []
	 end,
    P2 = case PAct#action.text of
	     "" -> P1;
	     Text -> P1 ++ [Text]
	 end,
    case PAct#action.table of
	[] -> P2;
	Table -> P2 ++ [Table]
    end.

prepare_act(F, ScnID, EID, Act) ->
    Scn = lists:nth(ScnID, F#feature.scenarios),
    case Scn#scenario.examples of
	       [] -> Act;
	       _ ->
		   HRow = lists:nth(1, Scn#scenario.examples),
		   ERow = lists:nth(EID + 1, Scn#scenario.examples),
		   try lists:zip(HRow, ERow) of
		       E ->
			   lists:foldl(
			     fun ({K,V}, A) ->
				     NewDesc = re:replace(A#action.desc,
							  "<" ++ K ++ ">",
							  V,
							  [{return, list}]),
				     A#action{desc = NewDesc}
			     end, Act, E)
		   catch
		       _ -> Act
		   end
	   end.

%%%===================================================================
%%% Util functions
%%%===================================================================
get_modstate(State, Mod) ->
    case dict:find(Mod,State) of
        {ok,ModState} -> ModState;
        error -> no_state
    end.

set_modstate(State, Mod, ModState) ->
    dict:store(Mod, ModState, State).

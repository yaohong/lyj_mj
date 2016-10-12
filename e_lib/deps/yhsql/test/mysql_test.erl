%% file: yhsql_test.erl
%% author: Yariv Sadan (yarivvv@gmail.com)
%% for license see COPYING

-module(yhsql_test).
-compile(export_all).

test() ->
    compile:file("/usr/local/lib/erlang/lib/yhsql/yhsql.erl"),
    compile:file("/usr/local/lib/erlang/lib/yhsql/yhsql_conn.erl"),
    
    %% Start the MySQL dispatcher and create the first connection
    %% to the database. 'p1' is the connection pool identifier.
    yhsql:start_link(p1, "localhost", "root", "password", "test"),

    %% Add 2 more connections to the connection pool
    yhsql:connect(p1, "localhost", undefined, "root", "password", "test",
		  true),
    yhsql:connect(p1, "localhost", undefined, "root", "password", "test",
		  true),
    
    yhsql:fetch(p1, <<"DELETE FROM developer">>),

    yhsql:fetch(p1, <<"INSERT INTO developer(name, country) VALUES "
		     "('Claes (Klacke) Wikstrom', 'Sweden'),"
		     "('Ulf Wiger', 'USA')">>),

    %% Execute a query (using a binary)
    Result1 = yhsql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result1: ~p~n", [Result1]),
    
    %% Register a prepared statement
    yhsql:prepare(update_developer_country,
		  <<"UPDATE developer SET country=? where name like ?">>),
    
    %% Execute the prepared statement
    yhsql:execute(p1, update_developer_country, [<<"Sweden">>, <<"%Wiger">>]),
    
    Result2 = yhsql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result2: ~p~n", [Result2]),
    
    yhsql:transaction(
      p1,
      fun() -> yhsql:fetch(<<"INSERT INTO developer(name, country) VALUES "
			    "('Joe Armstrong', 'USA')">>),
	       yhsql:fetch(<<"DELETE FROM developer WHERE name like "
			    "'Claes%'">>)
      end),

    Result3 = yhsql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result3: ~p~n", [Result3]),
    
    yhsql:prepare(delete_all, <<"DELETE FROM developer">>),

    {error, foo} = yhsql:transaction(
		     p1,
		     fun() -> yhsql:execute(delete_all),
			      throw({error, foo})
		     end),

    Result4 = yhsql:fetch(p1, <<"SELECT * FROM developer">>),
    io:format("Result4: ~p~n", [Result4]),
				    
    ok.
    
    

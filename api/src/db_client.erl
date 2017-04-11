-module(db_client).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-compile(export_all).                           % change later

%% TODO
init(_) ->
    odbc:start(),
    {ok, maps:new()}.

%% gen-server handler
handle_call(Msg, _From, State) ->
    case Msg of
        {all_info} ->
            {reply, get_all_info(), State};
        {prices} ->
            {reply, get_prices(), State};
        {quantity} ->
            {reply, get_quantity(), State};
        {info, Id} ->
            {reply, get_product_info(Id), State};
        {price, Id} ->
            {reply, get_price(Id), State};
        {quantity, Id} ->
            {reply, get_quantity(Id), State};
        _ ->
            {reply, {error, "unknown message"}, State}
    end.

terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen-server API functions
start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    register(db_client, Pid),
    {ok, db_client}.

all_info() -> gen_server:call(?MODULE, {all_info}).

prices() -> gen_server:call(?MODULE, {prices}).

quantity() -> gen_server:call(?MODULE, {quantity}).

product_info(Id) -> gen_server:call(?MODULE, {info, Id}).

price(Id) -> gen_server:call(?MODULE, {price, Id}).

quantity(Id) -> gen_server:call(?MODULE, {quantity, Id}).

%%TODO
exists(Id) -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server auxiliar functions
get_all_info() ->
    Sql = "SELECT * FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

get_prices() ->
    Sql = "SELECT NOMBRE, PRECIO FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

get_quantity() ->
    Sql = "SELECT NOMBRE, Unidades_por_producto FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

get_product_info(Id) ->
    Sql = "SELECT * FROM acm.PRODUCTO where ID = " ++ Id + ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

get_price(Id) ->
    Sql = "SELECT NOMBRE, PRECIO FROM acm.PRODUCTO where ID = " ++ Id ++ ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

get_quantity(Id) ->
    Sql = "SELECT NOMBRE, Unidades_por_producto FROM acm.PRODUCTO where ID = " ++ Id ++ ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect() ->
    ConnectionString = "DSN=test3;UID=root;PWD=holacaracola",
    {ok, Conn} = odbc:connect(ConnectionString, []),
    Conn.

%% function to test the connection (just for debuging)
test_connection() ->
    Sql = "SELECT 1;",
    Connection = connect(),
    case odbc:sql_query(Connection, Sql) of
        {selected, Columns, Results} ->
            io:format("Success!~n Columns: ~p~n Results: ~p~n",
                      [Columns, Results]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

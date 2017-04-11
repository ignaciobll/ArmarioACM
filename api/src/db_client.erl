-module(db_client).

-export([all_info/0, prices/0, quantity/0, product_info/1, price/1,
	 quantity/1]).

all_info() ->
    Sql = "SELECT * FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

prices() ->
    Sql = "SELECT NOMBRE, PRECIO FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

quantity() ->
    Sql = "SELECT NOMBRE, Unidades_por_producto FROM acm.PRODUCTO",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

product_info(Id) ->
    Sql = "SELECT * FROM acm.PRODUCTO where ID = " ++ Id + ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

price(Id) ->
    Sql = "SELECT NOMBRE, PRECIO FROM acm.PRODUCTO where ID = " ++ Id ++ ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

quantity(Id) ->
    Sql = "SELECT NOMBRE, Unidades_por_producto FROM acm.PRODUCTO where ID = " ++ Id ++ ";",
    Connection = connect(),
    odbc:sql_query(Connection, Sql).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect() ->
    ConnectionString = "DSN=your_dsn;UID=your_user;PWD=your_pwd",
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

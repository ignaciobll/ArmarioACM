CREATE TABLE COMPRA(
       ID INTEGER PRIMARY KEY NOT NULL,
       SUBTOTAL	      REAL NOT NULL,
       FECHA_COMPRA   DATE,
       CONCEPTO	      TEXT
);

CREATE TABLE PRODUCTO(
       ID INTEGER PRIMARY KEY NOT NULL,
       NOMBRE TEXT NOT NULL,
       Unidades_por_producto INTEGER NOT NULL,
       PRECIO REAL
);

CREATE TABLE COMPRA_PRODUCTO(
       ID INTEGER PRIMARY KEY NOT NULL,
       ID_COMPRA INTEGER,
       ID_PRODUCTO INTEGER,
       UNIDADES INTEGER NOT NULL,
       PRECIO_COMPRA REAL NOT NULL,
       FOREIGN KEY(ID_COMPRA) REFERENCES COMPRA(ID),
       FOREIGN KEY(ID_PRODUCTO) REFERENCES PRODUCTO(ID)
);

-- Trigger para el histórico del precio del produto.
-- CREATE TRIGGER HISTORICO_PRECIOS AFTER UPDATE OF PRECIO ON PRODUCTO BEGIN INSERT

CREATE TABLE PRECIOS(
       ID INTEGER PRIMARY KEY NOT NULL,
       ID_PRODUCTO INTEGER,
       PRECIO_VENTA REAL,
       FECHA_INICIO DATE,
       FOREIGN KEY(ID_PRODUCTO) REFERENCES PRODUCTO(ID)
);

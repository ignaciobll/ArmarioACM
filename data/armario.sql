CREATE DATABASE  IF NOT EXISTS `acm` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci */;
USE `acm`;
-- MySQL dump 10.13  Distrib 5.7.17, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: acm
-- ------------------------------------------------------
-- Server version	5.5.5-10.1.22-MariaDB

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `COMPRA`
--

DROP TABLE IF EXISTS `COMPRA`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `COMPRA` (
  `ID` int(11) NOT NULL,
  `SUBTOTAL` double NOT NULL,
  `FECHA_COMPRA` datetime DEFAULT NULL,
  `CONCEPTO` text COLLATE utf8mb4_unicode_ci,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `COMPRA`
--

LOCK TABLES `COMPRA` WRITE;
/*!40000 ALTER TABLE `COMPRA` DISABLE KEYS */;
/*!40000 ALTER TABLE `COMPRA` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `COMPRA_PRODUCTO`
--

DROP TABLE IF EXISTS `COMPRA_PRODUCTO`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `COMPRA_PRODUCTO` (
  `ID` int(11) NOT NULL,
  `ID_COMPRA` int(11) DEFAULT NULL,
  `ID_PRODUCTO` int(11) DEFAULT NULL,
  `UNIDADES` int(11) NOT NULL,
  `PRECIO_COMPRA` double NOT NULL,
  PRIMARY KEY (`ID`),
  KEY `ID_COMPRA` (`ID_COMPRA`),
  KEY `ID_PRODUCTO` (`ID_PRODUCTO`),
  CONSTRAINT `COMPRA_PRODUCTO_ibfk_1` FOREIGN KEY (`ID_COMPRA`) REFERENCES `COMPRA` (`ID`),
  CONSTRAINT `COMPRA_PRODUCTO_ibfk_2` FOREIGN KEY (`ID_PRODUCTO`) REFERENCES `PRODUCTO` (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `COMPRA_PRODUCTO`
--

LOCK TABLES `COMPRA_PRODUCTO` WRITE;
/*!40000 ALTER TABLE `COMPRA_PRODUCTO` DISABLE KEYS */;
/*!40000 ALTER TABLE `COMPRA_PRODUCTO` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `PRECIOS`
--

DROP TABLE IF EXISTS `PRECIOS`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `PRECIOS` (
  `ID` int(11) NOT NULL,
  `ID_PRODUCTO` int(11) DEFAULT NULL,
  `PRECIO_VENTA` double DEFAULT NULL,
  `FECHA_INICIO` datetime DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `ID_PRODUCTO` (`ID_PRODUCTO`),
  CONSTRAINT `PRECIOS_ibfk_1` FOREIGN KEY (`ID_PRODUCTO`) REFERENCES `PRODUCTO` (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `PRECIOS`
--

LOCK TABLES `PRECIOS` WRITE;
/*!40000 ALTER TABLE `PRECIOS` DISABLE KEYS */;
/*!40000 ALTER TABLE `PRECIOS` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `PRODUCTO`
--

DROP TABLE IF EXISTS `PRODUCTO`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `PRODUCTO` (
  `ID` int(11) NOT NULL,
  `NOMBRE` text COLLATE utf8mb4_unicode_ci NOT NULL,
  `Unidades_por_producto` int(11) NOT NULL,
  `PRECIO` double DEFAULT NULL,
  `BARCODE` text COLLATE utf8mb4_unicode_ci NOT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `PRODUCTO`
--

LOCK TABLES `PRODUCTO` WRITE;
/*!40000 ALTER TABLE `PRODUCTO` DISABLE KEYS */;
/*!40000 ALTER TABLE `PRODUCTO` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `VENTA`
--

DROP TABLE IF EXISTS `VENTA`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `VENTA` (
  `ID` int(11) NOT NULL,
  `FECHA` datetime DEFAULT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `VENTA`
--

LOCK TABLES `VENTA` WRITE;
/*!40000 ALTER TABLE `VENTA` DISABLE KEYS */;
/*!40000 ALTER TABLE `VENTA` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `VENTA_PRODUCTO`
--

DROP TABLE IF EXISTS `VENTA_PRODUCTO`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `VENTA_PRODUCTO` (
  `ID` int(11) NOT NULL,
  `CANTIDAD` int(11) DEFAULT '1',
  `ID_VENTA` int(11) DEFAULT NULL,
  `ID_PRODUCTO` int(11) DEFAULT NULL,
  PRIMARY KEY (`ID`),
  KEY `ID_VENTA` (`ID_VENTA`),
  KEY `ID_PRODUCTO` (`ID_PRODUCTO`),
  CONSTRAINT `VENTA_PRODUCTO_ibfk_1` FOREIGN KEY (`ID_VENTA`) REFERENCES `VENTA` (`ID`),
  CONSTRAINT `VENTA_PRODUCTO_ibfk_2` FOREIGN KEY (`ID_PRODUCTO`) REFERENCES `PRODUCTO` (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `VENTA_PRODUCTO`
--

LOCK TABLES `VENTA_PRODUCTO` WRITE;
/*!40000 ALTER TABLE `VENTA_PRODUCTO` DISABLE KEYS */;
/*!40000 ALTER TABLE `VENTA_PRODUCTO` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2017-04-08 12:29:54

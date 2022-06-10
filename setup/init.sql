--
-- PostgreSQL database dump
--

-- Dumped from database version 12.3
-- Dumped by pg_dump version 14.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: dymium; Type: SCHEMA; Schema: -; Owner: dymium
--

CREATE SCHEMA dymium;


ALTER SCHEMA dymium OWNER TO dymium;

--
-- Name: global; Type: SCHEMA; Schema: -; Owner: dymium
--

CREATE SCHEMA global;


ALTER SCHEMA global OWNER TO dymium;

--
-- Name: spoofcorp; Type: SCHEMA; Schema: -; Owner: dymium
--

CREATE SCHEMA spoofcorp;


ALTER SCHEMA spoofcorp OWNER TO dymium;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: customers; Type: TABLE; Schema: global; Owner: postgres
--

CREATE TABLE global.customers (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    company_name character varying(128) NOT NULL,
    schema_name character varying(36) NOT NULL,
    organization character varying(64) NOT NULL,
    domain character varying(128) NOT NULL
);


ALTER TABLE global.customers OWNER TO postgres;

--
-- Name: access; Type: TABLE; Schema: spoofcorp; Owner: dymium
--

CREATE TABLE spoofcorp.access (
    datascope_id character varying(36) NOT NULL,
    group_id character varying(36) NOT NULL
);


ALTER TABLE spoofcorp.access OWNER TO dymium;

--
-- Name: admincredentials; Type: TABLE; Schema: spoofcorp; Owner: postgres
--

CREATE TABLE spoofcorp.admincredentials (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    connection_id character varying(36) NOT NULL,
    username character varying(128) NOT NULL
);


ALTER TABLE spoofcorp.admincredentials OWNER TO postgres;

--
-- Name: connections; Type: TABLE; Schema: spoofcorp; Owner: postgres
--

CREATE TABLE spoofcorp.connections (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    address character varying(36) NOT NULL,
    port integer NOT NULL,
    name character varying(128) NOT NULL,
    database_type character varying(128) NOT NULL,
    use_tls boolean NOT NULL,
    description character varying(256),
    dbname character varying(128)
);


ALTER TABLE spoofcorp.connections OWNER TO postgres;

--
-- Name: datascopes; Type: TABLE; Schema: spoofcorp; Owner: dymium
--

CREATE TABLE spoofcorp.datascopes (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    name character varying(128) NOT NULL
);


ALTER TABLE spoofcorp.datascopes OWNER TO dymium;

--
-- Name: passwords; Type: TABLE; Schema: spoofcorp; Owner: postgres
--

CREATE TABLE spoofcorp.passwords (
    id character varying(36) NOT NULL,
    password character varying(36) NOT NULL
);


ALTER TABLE spoofcorp.passwords OWNER TO postgres;

--
-- Name: refs; Type: TABLE; Schema: spoofcorp; Owner: dymium
--

CREATE TABLE spoofcorp.refs (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    schem character varying(128) NOT NULL,
    tabl character varying(128) NOT NULL,
    col character varying(128) NOT NULL
);


ALTER TABLE spoofcorp.refs OWNER TO dymium;

--
-- Name: tables; Type: TABLE; Schema: spoofcorp; Owner: dymium
--

CREATE TABLE spoofcorp.tables (
    id character varying(36) DEFAULT public.uuid_generate_v4() NOT NULL,
    datascope_id character varying(36) NOT NULL,
    connection_id character varying(64) NOT NULL,
    schem character varying(128) NOT NULL,
    tabl character varying(128) NOT NULL,
    action character varying(32) NOT NULL,
    col character varying(128) NOT NULL,
    typ character varying(32) NOT NULL,
    "position" integer,
    reference character varying(36),
    semantics character varying(32) NOT NULL
);


ALTER TABLE spoofcorp.tables OWNER TO dymium;

--
-- Name: customers customers_pkey; Type: CONSTRAINT; Schema: global; Owner: postgres
--

ALTER TABLE ONLY global.customers
    ADD CONSTRAINT customers_pkey PRIMARY KEY (id);


--
-- Name: admincredentials admincredentials_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: postgres
--

ALTER TABLE ONLY spoofcorp.admincredentials
    ADD CONSTRAINT admincredentials_pkey PRIMARY KEY (id);


--
-- Name: connections connections_name_key; Type: CONSTRAINT; Schema: spoofcorp; Owner: postgres
--

ALTER TABLE ONLY spoofcorp.connections
    ADD CONSTRAINT connections_name_key UNIQUE (name);


--
-- Name: connections connections_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: postgres
--

ALTER TABLE ONLY spoofcorp.connections
    ADD CONSTRAINT connections_pkey PRIMARY KEY (id);


--
-- Name: datascopes datascopes_name_key; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.datascopes
    ADD CONSTRAINT datascopes_name_key UNIQUE (name);


--
-- Name: datascopes datascopes_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.datascopes
    ADD CONSTRAINT datascopes_pkey PRIMARY KEY (id);


--
-- Name: passwords passwords_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: postgres
--

ALTER TABLE ONLY spoofcorp.passwords
    ADD CONSTRAINT passwords_pkey PRIMARY KEY (id);


--
-- Name: refs refs_col_key; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.refs
    ADD CONSTRAINT refs_col_key UNIQUE (col);


--
-- Name: refs refs_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.refs
    ADD CONSTRAINT refs_pkey PRIMARY KEY (id);


--
-- Name: tables tables_datascope_id_connection_id_schem_action_col_key; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.tables
    ADD CONSTRAINT tables_datascope_id_connection_id_schem_action_col_key UNIQUE (datascope_id, connection_id, schem, action, col);


--
-- Name: tables tables_pkey; Type: CONSTRAINT; Schema: spoofcorp; Owner: dymium
--

ALTER TABLE ONLY spoofcorp.tables
    ADD CONSTRAINT tables_pkey PRIMARY KEY (id);


--
-- Name: TABLE customers; Type: ACL; Schema: global; Owner: postgres
--

GRANT ALL ON TABLE global.customers TO dymium;


--
-- Name: TABLE admincredentials; Type: ACL; Schema: spoofcorp; Owner: postgres
--

GRANT ALL ON TABLE spoofcorp.admincredentials TO dymium;


--
-- Name: TABLE connections; Type: ACL; Schema: spoofcorp; Owner: postgres
--

GRANT ALL ON TABLE spoofcorp.connections TO dymium;


--
-- Name: TABLE passwords; Type: ACL; Schema: spoofcorp; Owner: postgres
--

GRANT ALL ON TABLE spoofcorp.passwords TO dymium;


--
-- PostgreSQL database dump complete
--


-- Initialize the BroScore PostgreSQL schema.
-- Types needed by tables are defined above them.

begin;

-- User

drop table if exists "user" cascade;

create table "user" (
	"id" bigserial primary key,
	"username" varchar(64) constraint "unique_user_name" unique,
	"password_hash" bytea,
	"activity" bytea,
	"effect" bytea,
	constraint "username_not_null" check (not ("username" is null)),
	constraint "password_hash_not_null" check (not ("password_hash" is null))
);

-- Person

drop table if exists "person" cascade;

create table "person" (
	"id" bigserial primary key,
	"name" varchar(256),
	"context_id" bigint,
	"score" bytea,
	"activity" bytea,
	constraint "name_not_null" check (not ("name" is null)),
	constraint "context_not_null" check (not ("context_id" is null)),
	constraint "score_not_null" check (not ("score" is null)),
	constraint "unique_person_name_context" unique ("name", "context_id")
);

-- Contex

drop table if exists "context" cascade;

create table "context" (
	"id" bigserial primary key,
	"name" varchar(256) constraint "unique_context_name" unique,
	"description" text,
	constraint "name_not_null" check (not ("name" is null))
);

drop table if exists "score_inc" cascade;

create table "score_inc" (
	"user_id" bigint,
	"person_id" bigint,
	"activity" bytea,
	"delta" bytea,
	"range" interval,
	constraint "user_id_not_null" check (not ("user_id" is null)),
	constraint "person_id_not_null" check (not ("person_id" is null)),
	constraint "activity_not_null" check (not ("activity" is null)),
	constraint "delta_not_null" check (not ("delta" is null)),
	constraint "range_not_null" check (not ("range" is null))
);

-- Foreign Keys

alter table "person" add foreign key ("context_id") references "context";
alter table "score_inc" add foreign key ("user_id") references "user";
alter table "score_inc" add foreign key ("person_id") references "person";

-- Indices

create index "user_id_index" on "user" ("id");
create index "user_username_index" on "user" ("username");

create index "person_id_index" on "person" ("id");
create index "person_name_index" on "person" ("name");
create index "person_context_id_index" on "person" ("context_id");

create index "context_id_index" on "context" ("id");
create index "context_name_index" on "context" ("name");

create index "score_inc_user_id_index" on "score_inc" ("user_id");
create index "score_inc_person_id_index" on "score_inc" ("person_id");
create index "score_inc_range_index" on "score_inc" ("range");

commit;

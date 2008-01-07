create table seen_articles (
	title varchar(128) not null
);

create table interesting_pages (
	title varchar(128) not null,
	article_text text not null,
	latitude number(3, 7),
	longitude number(3, 7)
);
create unique index idx_interesting_title on interesting_pages(title);

indexing
	description: "The Replicator...";
	version: "$Revision: 1.6 $";

class MREPLICATE

creation make

feature

	make is
		local
			s, d: PG;
			rep: PG_REPLICATE;
		do
			!!s.make;
			s.set_dbname("modems");
			s.set_host("propaganda.spy.net");
			s.connect;

			!!d.make;
			d.set_dbname("modems");
			d.set_host("bleu");
			d.connect;

			!!rep.make(s, d);

			rep.full;
		end
end

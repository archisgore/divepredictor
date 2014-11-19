

-record(divelocation, {
		latitude=-1,
		longitude=-1,
		address=""
	}).

-record(divesite, {
		id,
		name,
		noaaTideStationId=0,
		noaaCurrentStationId=0,
		location=#divelocation{}
	}).

-record(divesolution, {
	siteId,
	time,
	length
	}).

-record(current, {
		stationId,
		dateTime,
		type,
		magnitude
	}).

-record(tide, {
		stationId,
		dateTime,
		type,
		magnitude
	}).
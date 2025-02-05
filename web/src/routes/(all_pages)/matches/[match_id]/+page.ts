import { redirect } from '@sveltejs/kit';
import type { PageLoad } from './$types';

export const load: PageLoad = ({ params }) => {
	if (params.match_id === undefined || params.match_id === null) redirect(308, '/matches');

	const sample = `{"gameType":"","messageType":"gameStart","numberOfPlayers":2}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":0,"response":{"move":[0,0]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":1,"response":{"move":[1,0]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":0,"response":{"move":[2,0]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":1,"response":{"move":[0,1]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":0,"response":{"move":[1,1]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":1,"response":{"move":[2,1]}}]}
{"messageType":"playerResponses","responses":[{"errorMessage":"","errorType":"","isValid":true,"player":0,"response":{"move":[0,2]}}]}
{"messageType":"gameEnd","scores":[{"player":0,"score":1},{"player":1,"score":0}]}`;

	return { gameHistory: sample };
};

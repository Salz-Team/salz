import { Temporal } from 'temporal-polyfill';
import type { Bot } from './bot.model';

export type MatchResult = {
	id: string;
	timestamp: Temporal.Instant;
	bot1: Bot;
	bot2: Bot;
	winner: 'bot1' | 'bot2';
};

import { localWritable } from '$lib/store';
import { derived } from 'svelte/store';

export interface UserStore {
	// Bigint in database, but we'll treat it as a string
	id: string;
	username: string;
	iconUrl: string;
}

export const initialValue = { id: '', username: '', iconUrl: '' };

export const userStore = localWritable('userStore')<UserStore>(initialValue);

export const isLoggedIn = derived(userStore, ($userStore) => Boolean($userStore?.id));

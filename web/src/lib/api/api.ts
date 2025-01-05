import { get } from '$lib/req';
import { None } from 'rust-optionals';

export const getHealth = (apiBaseurl: string) =>
	get(new URL('/health', apiBaseurl), None(), {}, { mode: 'cors' });

export const login = async (apiBaseurl: string): Promise<void> => {
	await get(new URL('/login', apiBaseurl), None(), {}, { redirect: 'follow', mode: 'cors' }).then(
		(resp) => {
			console.log(resp);
		},
	);
};

export const getMe = (apiBaseurl: string) =>
	get(new URL('/users/me', apiBaseurl), None(), {}, { mode: 'cors', credentials: 'include' });

export const getUsers = (apiBaseurl: string) =>
	get(new URL('/users', apiBaseurl), None(), {}, { mode: 'cors', credentials: 'include' });

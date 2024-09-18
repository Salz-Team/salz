import { get } from '$lib/req';
import { None } from 'rust-optionals';

export const getHealth = (apiBaseurl: string) =>
	get(new URL('/health', apiBaseurl), None(), {}, { mode: 'cors' });

export const login = async (apiBaseurl: string): Promise<string | null> => {
	const resp = await get(
		new URL('/login', apiBaseurl),
		None(),
		{},
		{ redirect: 'manual', mode: 'cors' },
	);
	if (resp.isOk()) {
		return resp.unwrap().headers.get('location');
	} else {
		return null;
	}
};

export const getMe = (apiBaseurl: string) =>
	get(new URL('/users/me', apiBaseurl), None(), {}, { mode: 'cors' });

export const getUsers = (apiBaseurl: string) =>
	get(new URL('/users', apiBaseurl), None(), {}, { mode: 'cors' });

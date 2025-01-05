import { Err, Ok, type Option, type Result } from 'rust-optionals';

export type Resp<T extends Record<string, unknown>> = {
	headers: Headers;
	body: T;
};

export const req = async <T extends Record<string, unknown>>(
	method: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH' | 'OPTION',
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => {
	try {
		const resp = await fetch(uri, {
			method,
			body: body.isNone() ? undefined : JSON.stringify(body.unwrap()),
			headers: {
				'Content-Type': 'application/json',
				...headers,
			},
			...fetchOptions,
		});
		const respBody = resp.body ? await resp.json() : {};
		const respStatusClass = (resp.status / 100).toPrecision(1);
		switch (respStatusClass) {
			case '4':
			case '5':
				return Err(respBody);
			default:
				return Ok({ headers: resp.headers, body: respBody as T });
		}
	} catch (e) {
		if (e instanceof Error) return Err(e);
		throw new Error(`Unexpected problem: ${e}`);
	}
};

export const get = <T extends Record<string, unknown>>(
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => req('GET', uri, body, headers, fetchOptions);

export const post = <T extends Record<string, unknown>>(
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => req('POST', uri, body, headers, fetchOptions);

export const del = <T extends Record<string, unknown>>(
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => req('DELETE', uri, body, headers, fetchOptions);

export const patch = <T extends Record<string, unknown>>(
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => req('PATCH', uri, body, headers, fetchOptions);

export const put = <T extends Record<string, unknown>>(
	uri: string | URL,
	body: Option<BodyInit>,
	headers: Record<string, unknown> = {},
	fetchOptions: RequestInit = {},
): Promise<Result<Resp<T>, Error>> => req('PUT', uri, body, headers, fetchOptions);

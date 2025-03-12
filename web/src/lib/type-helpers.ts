export type ValuesOf<T extends Record<string | number | symbol, unknown>> = T[keyof T];

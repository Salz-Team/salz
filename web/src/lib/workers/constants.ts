export type STATUSES = 'processing' | 'idle' | 'error' | 'completed';
export const STATUS_PROCESSING: STATUSES = 'processing';
export const STATUS_IDLE: STATUSES = 'idle';
export const STATUS_ERROR: STATUSES = 'error';
export const STATUS_COMPLETED: STATUSES = 'completed';

export type TASK_CONTROLS = 'start' | 'stop';
export const TASK_START: TASK_CONTROLS = 'start';
export const TASK_STOP: TASK_CONTROLS = 'stop';

import type { ValuesOf } from "$lib/type-helpers";

export const TaskControls = {
    Init: "init",
    Page: "page",
    End: "end"
} as const;
export type TaskControlsKey = ValuesOf<typeof TaskControls>;
export const TASK_INIT = "init";
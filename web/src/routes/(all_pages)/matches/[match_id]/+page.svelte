<script lang="ts">
	import { onDestroy, onMount } from 'svelte';
	import { browser } from '$app/environment';
	import type { PageProps } from './$types';
	import { TASK_START, TASK_STOP } from '$workers/constants';
	import type { Response } from '$workers/game-history-parsers/tic-tac-toe.types';

	let { data }: PageProps = $props();

	let worker: Worker | null = $state(null);

	async function initWebWorker() {
		if (browser && window.Worker) {
			const TicTacToeWorker = await import('$workers/game-history-parsers/tic-tac-toe.ts?worker');
			worker = new TicTacToeWorker.default();
		}
	}

	function runWorker() {
		if (worker) {
			worker.postMessage({
				task: TASK_START,
				gameHistory: data.gameHistory,
			});
			worker.onmessage = (e: MessageEvent<Response>) => {
				switch (e.data.type) {
					case 'status':
						console.log(new Date().getTime(), e.data.status);
						console.log(new Date().getTime(), e.data.status.message);

						if (e.data.status.roundsProcessed === 5) {
              console.log(new Date().getTime(), "Stopping worker");
							worker!.postMessage({ task: TASK_STOP });
						}
						break;
					case 'error':
						console.error(e.data);
				}
			};
		}
	}

	function terminateWorker() {
		if (worker) {
			worker.postMessage({ task: TASK_STOP });
			worker.terminate();
		}
	}

	onMount(() => {
		initWebWorker();
	});

	onDestroy(() => {
		terminateWorker();
	});
</script>

<button disabled={!Boolean(worker)} onclick={runWorker}>Run!</button>

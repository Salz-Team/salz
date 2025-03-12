<script lang="ts">
	import { onDestroy, onMount } from 'svelte';
	import { browser } from '$app/environment';
	import type { PageData } from './$types';
	import { TASK_START, TASK_STOP } from '$workers/game-history-parsers/constants';
	import type { Response } from '$workers/game-history-parsers/tic-tac-toe.types';
	import Range from '$lib/components/Range.svelte';
	import { getColourscheme } from '$lib/colorscheme';
	import { STATUS_PROCESSING } from '$workers/constants';
	import type { GameHistoryLine } from '$lib/models/game_history.model';

	let { data }: { data: PageData } = $props();

	let canvas: HTMLCanvasElement | null = null;
	let frameNumber: number = $state(-1);
	let totalFrames: number | null = $state(null);
	let isLoading: boolean = $state(true);
	let currentlyProcessedFrame: number | null = $state(null);

	let historyParsingWorker: Worker | null = $state(null);
	let gameVisualizer: Worker | null = $state(null);

	async function init() {
		const gameHistory = data.gameHistory.split('\n').map((s) => JSON.parse(s) as GameHistoryLine);
		totalFrames = gameHistory.filter((l) => l.messageType === 'playerResponses').length;

		if (browser && window.Worker) {
			const gameMetadata = gameHistory[0];
			if (Object.hasOwn(gameMetadata, 'gameType')) {
				switch (gameMetadata['gameType']) {
					case 'tic-tac-toe':
					case '':
						historyParsingWorker = new (
							await import('$workers/game-history-parsers/tic-tac-toe.ts?worker')
						).default();
						gameVisualizer = new (
							await import('$workers/game-visualizers/tic-tac-toe.ts?worker')
						).default();

						break;
					default:
						console.error(gameMetadata['gameType'], 'is not a known game type');
				}
			} else {
				console.error("Game history does not contain a 'gameType'.");
			}
		} else {
			console.error('Web workers are not supported by your browser');
		}
	}

	function runWorker() {
		if (historyParsingWorker) {
			historyParsingWorker.postMessage({
				task: TASK_START,
				matchId: data.matchId,
				gameHistory: data.gameHistory,
			});
		}
	}

	function terminateWorker() {
		if (historyParsingWorker) {
			historyParsingWorker.postMessage({ task: TASK_STOP });
			historyParsingWorker.terminate();
		}

		if (gameVisualizer) {
			gameVisualizer.terminate();
		}
	}

	onMount(async () => {
		const colourscheme = getColourscheme(document.body);

		await init().then(() => {
			if (historyParsingWorker) {
				historyParsingWorker.onmessage = (e: MessageEvent<Response>) => {
					switch (e.data.type) {
						case 'status':
							if (e.data.status.type === STATUS_PROCESSING) {
								currentlyProcessedFrame = e.data.status.roundsProcessed;
								console.log(new Date().getTime(), e.data.status.roundsProcessed);
							}
							break;
						case 'completed':
							frameNumber = 0;
							currentlyProcessedFrame = null;
							totalFrames = e.data.completed.gameInfo.totalFrames;

							if (canvas) {
								const offscreenCanvas = canvas.transferControlToOffscreen();
								gameVisualizer!.postMessage(
									{
										type: 'init',
										init: {
											canvas: offscreenCanvas,
											canvasHeight: 500,
											canvasWidth: 500,
											storePrefix: e.data.completed.historyPrefix,
											totalFrames: e.data.completed.gameInfo.totalFrames,
											colourscheme,
										},
									},
									[offscreenCanvas],
								);

								isLoading = false;
							} else {
								console.error('No canvas is found in the DOM.');
							}
							break;
						case 'error':
							console.error(e.data);
					}
				};
			}
		});

		runWorker();
	});

	onDestroy(() => {
		terminateWorker();
	});

	$effect(() => {
		if (frameNumber < 0) frameNumber = 0;
		if (totalFrames !== null && frameNumber > totalFrames - 1) frameNumber = totalFrames - 1;

		if (gameVisualizer !== null) {
			gameVisualizer.postMessage({
				type: 'page',
				page: frameNumber,
			});
		}
	});
</script>

<main>
	{#if isLoading}
    <div class="loading-scene">
      Loading...<br />
      <br />
      {#if currentlyProcessedFrame !== null}
        Processing {currentlyProcessedFrame} / {totalFrames} frames
      {/if}
    </div>
	{/if}

	<div class="visualizer-container">
		<canvas bind:this={canvas}></canvas>

		{#if !isLoading}
			{#if totalFrames !== null}
				<Range bind:value={frameNumber} min={0} max={totalFrames - 1} />
			{/if}
		{/if}
	</div>
</main>

<style>
	main {
		display: flex;
		justify-content: center;
    align-items: center;
    flex-direction: column;
	}

  .loading-scene {
    text-align: center;
  }

  .visualizer-container {
    width: 70%;
  }

  .visualizer-container > canvas {
    margin: 0 auto;
  }
</style>

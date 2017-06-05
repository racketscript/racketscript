// Continuation Marks
import * as Pair from "./pair.js"
import * as Symbol from "./symbol.js"
import * as $ from "./lib.js";

let __frames = false;
let __prompts = new Map();
let __async_callback_wrappers = [];
let __defaultContinuationPromptTag
    = makeContinuationPromptTag(Symbol.make("default"));

let HASH = $.hashEq;

/* --------------------------------------------------------------------------*/

export function init() {
    __frames = Pair.Empty;
    savePrompt(__defaultContinuationPromptTag);
    enterFrame();
}

export function registerAsynCallbackWrapper(w) {
    __async_callback_wrappers.push(w);
}

export function defaultContinuationPromptTag() {
    return __defaultContinuationPromptTag;
}

init();

/* --------------------------------------------------------------------------*/
// Prompts are stored in __prompts map. It is a map between
// ContinuationPromptTag to [Frame]. The value is used to filter out frames
// until given prompt. The most recent frame marked with a prompt is last
// item in the array mapped to the prompt tag.

//TODO: Handle default-continution-prompt-tag

function ContinuationPromptTag(tag) {
    this.tag = tag;
    return this;
}

export function AbortCurrentContinuation(promptTag, handlerArgs) {
    this.name = "abort-current-continuation";
    this.promptTag = promptTag;
    this.handlerArgs = handlerArgs;

    this.stack = (new Error()).stack;
    if (Error.captureStackTrace) {
        Error.captureStackTrace(this, this.constructor);
    } else {
        this.stack = (new Error()).stack;
    }
}
AbortCurrentContinuation.prototype = Object.create(Error.prototype);
AbortCurrentContinuation.prototype.constructor = AbortCurrentContinuation;

function savePrompt(promptTag) {
    let promptVal = __prompts.get(promptTag);
    if (promptVal === undefined) {
	promptVal = []
	__prompts.set(promptTag, promptVal);
    }
    // Most recent prompt is at the end of array.
    promptVal.push(__frames.hd)
}

function deleteCurrentPrompt(promptTag) {
    let promptVal = __prompts.get(promptTag);
    if (promptVal === undefined) {
	throw $.racketCoreError("No corresponding tag in continuation!");
    }
    promptVal.pop()
    if (promptVal.length === 0) {
	__prompts.delete(promptTag);
    }
}

function getPromptFrame(promptTag) {
    if (promptTag === undefined) {
	return promptTag;
    } else {
	let result = __prompts.get(promptTag);
	return (result && result[result.length-1])
	    || undefined;
    }
}

export function makeContinuationPromptTag(sym) {
    return new ContinuationPromptTag(sym);
}

export function isContinuationPromptTag(tag) {
    return tag instanceof ContinuationPromptTag;
}

export function callWithContinuationPrompt(proc, promptTag, handler, ...args) {
    promptTag = promptTag || __defaultContinuationPromptTag;
    try {
	savePrompt(promptTag);
	return proc.apply(null, args);
    } catch (e) {
	if (e instanceof AbortCurrentContinuation &&
	    e.promptTag === promptTag) {
	    return handler.apply(null, e.handlerArgs);
	} else {
	    throw e;
	}
    } finally {
	deleteCurrentPrompt(promptTag);
    }
}

/* --------------------------------------------------------------------------*/

export function getFrames() {
    return __frames;
}

export function updateFrame(newFrames, oldFrames) {
    if (__frames !== oldFrames) {
	throw new Error("current frame doesn't match with old frame");
    }
    return __frames = newFrames;
}

export function enterFrame() {
    __frames = Pair.make({}, __frames);
    return __frames;
}

export function setMark(key, value) {
    let frame = __frames.hd;
    frame[HASH(key)] = value;
}

export function getContinuationMarks(promptTag) {
    promptTag = promptTag || __defaultContinuationPromptTag;
    let frames = __frames;
    let promptFrame = getPromptFrame(promptTag);
    if (promptFrame === undefined &&
	promptTag !== __defaultContinuationPromptTag) {
	throw $.racketCoreError("No corresponding tag in continuation!");
    }

    let result = [];
    while (!Pair.isEmpty(frames)) {
	if (frames.hd === promptFrame) {
	    break;
	}
	result.push(frames.hd);
	frames = frames.tl;
    }
    return result;
}

export function getMarks(framesArr, key, promptTag) {
    promptTag = promptTag || __defaultContinuationPromptTag;
    let keyHash = HASH(key);
    let promptFrame = getPromptFrame(promptTag);

    let result = []
    for (let ii = 0; ii < framesArr.length; ++ii) {
	//FIXME: for-of requires polyfill
	let fr = framesArr[ii];
	if (keyHash in fr) {
	    if (fr === promptFrame) {
		break;
	    }
	    result.push(fr[keyHash]);
	}
    }
    return Pair.listFromArray(result);
}

//TODO: Used by parameterization. Add test cases around promptTag
// and parameterization and check if that if this needs 
export function getFirstMark(frames, key, noneV) {
    let keyHash = HASH(key);
    return Pair.listFind(frames, (fr) => {
	if (keyHash in fr) {
	    return fr[keyHash];
	}
    }) || noneV;
}

export function wrapWithContext(fn) {
    return (function (currentFrames) {
	let state = {};
	__async_callback_wrappers.forEach((w) => w.onCreate(state));
	return function (...args) {
	    init();
	    __async_callback_wrappers.forEach((w) => w.onInvoke(state));
	    try {
		return fn.apply(null, args);
	    } finally {
		/* This callback/coroutine is finished */
		__frames = undefined;
	    }
	}
    })(__frames);
}

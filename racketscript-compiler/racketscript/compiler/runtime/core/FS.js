import { fromString } from './path.js';

export const currentDir = () => fromString(process.cwd());

/**
 * This is provided as documentation only, it is not actually used by any code.
 *
 * TODO: When migrating to TypeScript, this should be converted to a mixin.
 */
export interface Printable {
    displayNativeString(out: Ports.NativeOutputStringPort);
    writeNativeString(out: Ports.NativeOutputStringPort);
    printNativeString(out: Ports.NativeOutputStringPort);

    displayUString(out: Ports.UStringOutputPort);
    writeUString(out: Ports.UStringOutputPort);
    printUString(out: Ports.UStringOutputPort);
}

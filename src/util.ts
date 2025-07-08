export function numberToColor(num: number): string  {
    if (num === 0) return "transparent";

    const exponent = Math.log2(num);
    const hue = (exponent * 40) % 360; // Te da un color cíclico distinto
    const saturation = 70;
    const lightness = 60;

    return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

export function delay(milliseconds: number) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}
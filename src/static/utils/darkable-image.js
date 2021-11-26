const darkmodeMedia = '(prefers-color-scheme: dark)';

class DarkmodeIcon extends HTMLElement {
    connectedCallback() {
        this.img = document.createElement('img');
        this.img.src = this.src;
        this.img.alt = this.alt;
        for (let attr of this.attributes) {
            this.img.setAttribute(attr.name, attr.value);
        }

        this.appendChild(this.img);

        this.classList.add('ui-img-dark');

        this.updateImageSource(window.matchMedia(darkmodeMedia));
    }

    updateImageSource(isDarkMode) {
        this.img.src = renameFile(this.img.src, isDarkMode);
    }
}

customElements.define('atb-img-dark', DarkmodeIcon);

const mql = window.matchMedia(darkmodeMedia);
updateAllDarkmodeIcons(mql);
mql.addEventListener('change', updateAllDarkmodeIcons);

function updateAllDarkmodeIcons(e) {
    const elements = Array.from(document.querySelectorAll('atb-img-dark'));
    console.log(elements);
    elements.forEach(function (img) {
        img.updateImageSource(e.matches);
    });
}

function renameFile(filename, isDarkMode) {
    if (!isDarkMode) {
        return filename.replace('-dark.', '.');
    }
    return filename.replace(/\.(\w{3})$/, '-dark.$1');
}

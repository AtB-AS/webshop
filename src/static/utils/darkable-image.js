const darkmodeMedia = '(prefers-color-scheme: dark)';

class ImageThemed extends HTMLElement {
    connectedCallback() {
        const isDarkMode = window.matchMedia(darkmodeMedia).matches;
        this.img = document.createElement('img');
        this.img.src = renameFile(this.src, isDarkMode);
        this.img.alt = this.alt;
        for (let attr of this.attributes) {
            if (attr.name === 'class') continue;
            this.img.setAttribute(attr.name, attr.value);
        }

        this.appendChild(this.img);
        this.classList.add('ui-img-dark');
    }

    updateImageSource(isDarkMode) {
        this.img.src = renameFile(this.img.src, isDarkMode);
    }
}

customElements.define('atb-img-themed', ImageThemed);

const mql = window.matchMedia(darkmodeMedia);
updateAllThemedIcons(mql);
mql.addEventListener('change', updateAllThemedIcons);

function updateAllThemedIcons(e) {
    const elements = Array.from(document.querySelectorAll('atb-img-themed'));
    elements.forEach(function (img) {
        img.updateImageSource(e.matches);
    });
}

function renameFile(filename, isDarkMode) {
    if (!isDarkMode) {
        return filename.replace('-dark.', '.');
    }
    return filename.replace(/\.(\w{3,4})$/, '-dark.$1');
}

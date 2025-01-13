const fromBase64 = base64 =>
    Uint8Array.from(atob(base64), c => c.charCodeAt(0))

const request = (url, data) => fetch(url, {
    method: "POST",
    headers: {
        "Content-Type": "application/json",
    },
    body: JSON.stringify(data),
})

const register = async () => {
    const response = await request("/register/start", {
        name: document.getElementById("name").value,
        displayName: document.getElementById("displayName").value,
    })
    if (response.ok) {
        const options = await response.json()
        console.log(options)
        const publicKey = {
            ...options,
            user: {
                ...options.user,
                id: fromBase64(options.user.id),
            },
            challenge: fromBase64(options.challenge),
        }
        credentials = await navigator.credentials.create({ publicKey })
        console.log(credentials)
        request("/register/finish", JSON.stringify(credentials))
    }
}

function authenticate() {
    console.log("authenticate")
}

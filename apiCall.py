import http.client

conn = http.client.HTTPSConnection("api.producthunt.com")

headers = {
    'accept': "application/json",
    'content-type': "application/json",
    'authorization': "Bearer ac38d4f8e02839f8902fcc988d8bd19e315582e496350f27873e9469a613040f",
    'host': "api.producthunt.com",
    'cache-control': "no-cache",
    'postman-token': "d5e61da7-c155-883d-c695-d7df81d6923e"
    }

conn.request("GET", "/v1/posts/1/votes?id=82825", headers=headers)

res = conn.getresponse()
data = res.read()

print(data.decode("utf-8"))